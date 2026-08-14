"""Request handlers for the Redux OData mock server.

This module implements the Command pattern to decompose the monolithic
dispatch_request() function into focused, testable handler classes.
Each handler is responsible for a single type of request following
the Single Responsibility Principle.
"""
from __future__ import annotations

import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, Dict, List, Match, Optional, Tuple, TYPE_CHECKING, Callable
from urllib.parse import parse_qs

import serve_config

from . import config
from . import state
from . import odata_format
from . import resolvers
from . import query
from . import draft_service
from . import crud
from .patterns import Patterns
from .enums import (
    DraftOperation,
    ErrorCode,
    ErrorSeverity,
    NavigationProperty,
    EntitySet,
    HttpStatus,
    RESULT_CODE_SATISFACTORY,
    ZERO_GUID,
)

if TYPE_CHECKING:
    from .di_container import ServiceContainer


@dataclass
class RequestContext:
    """Encapsulates request context for handlers."""
    method: str
    url_path: str
    query_params: Dict[str, Any]
    body: Optional[Dict[str, Any]]
    headers: Dict[str, str]
    
    @classmethod
    def from_raw(cls, method: str, rel_url: str, body: Optional[Dict], headers: Optional[Dict]) -> RequestContext:
        """Create RequestContext from raw request data."""
        parsed = parse_qs(rel_url.split('?')[1]) if '?' in rel_url else {}
        query_params = {k: v[0] if len(v) == 1 else v for k, v in parsed.items()}
        
        return cls(
            method=method,
            url_path=rel_url.split('?')[0],
            query_params=query_params,
            body=body,
            headers=headers or {}
        )


def _guid_param_factory(query_params: Dict[str, Any]) -> Callable[[str], str]:
    """Factory for GUID parameter extraction with validation."""
    def _guid_param(name: str) -> str:
        value = query_params.get(name, '')
        if not value:
            raise ValueError(f"Missing required parameter: {name}")
        return value
    return _guid_param


class ResponseBuilder:
    """Fluent builder for OData responses."""
    
    @staticmethod
    def ok(data: Any, content_type: str = "application/json") -> Tuple[int, Any, str]:
        """200 OK response."""
        return (HttpStatus.OK, {"d": data} if isinstance(data, dict) else data, content_type)
    
    @staticmethod
    def created(data: Any) -> Tuple[int, Any, str]:
        """201 Created response."""
        return (HttpStatus.CREATED, {"d": data}, "application/json")
    
    @staticmethod
    def no_content() -> Tuple[int, None, str]:
        """204 No Content response."""
        return (HttpStatus.NO_CONTENT, None, "application/json")
    
    @staticmethod
    def not_found(message: str = "Not found") -> Tuple[int, Dict, str]:
        """404 Not Found response."""
        return (HttpStatus.NOT_FOUND, {
            "error": {"message": {"value": message}}
        }, "application/json")
    
    @staticmethod
    def bad_request(code: str, message: str, severity: str = "error") -> Tuple[int, Dict, str]:
        """400 Bad Request with SAP Gateway error format."""
        return (HttpStatus.BAD_REQUEST, {
            "error": {
                "code": code,
                "message": {"value": message, "lang": "ru"},
            }
        }, "application/json")
    
    @staticmethod
    def conflict(code: str, message: str) -> Tuple[int, Dict, str]:
        """409 Conflict (draft locked)."""
        return (HttpStatus.BAD_REQUEST_DRAFT_LOCKED, {
            "error": {
                "code": code,
                "message": {"value": message, "lang": "ru"},
            }
        }, "application/json")
    
    @staticmethod
    def precondition_failed() -> Tuple[int, Dict, str]:
        """412 Precondition Failed."""
        return (HttpStatus.PRECONDITION_FAILED, {
            "error": {"message": {"value": "Precondition Failed"}}
        }, "application/json")
    
    @staticmethod
    def validation_error(missing_fields: List[Tuple[str, str]], missing_children: List[Dict], root_id: str) -> Tuple[int, Dict, str]:
        """400 Validation error with field-level details."""
        all_labels = [label for _f, label in missing_fields] + [c["label"] for c in missing_children]
        
        return (HttpStatus.BAD_REQUEST, {
            "error": {
                "code": "VALIDATION_ERROR",
                "message": {"value": "Не заполнены обязательные поля: " + ", ".join(all_labels)},
                "innererror": {
                    "errordetails": [
                        {
                            "code": "VALIDATION_ERROR",
                            "message": {"value": f"{label}: обязательное поле"},
                            "severity": "error",
                            "target": field,
                        }
                        for field, label in missing_fields
                    ] + [
                        {
                            "code": "VALIDATION_ERROR",
                            "message": {"value": f"{c['label']}: обязательное поле"},
                            "severity": "error",
                            "target": "%s(RootId='%s',%s='%s')/%s" % (
                                c["nav"], root_id, c["id_prop"], c["item_id"], c["field"]
                            ),
                        }
                        for c in missing_children
                    ]
                }
            }
        }, "application/json")


class BaseHandler(ABC):
    """Abstract base class for request handlers."""
    
    @abstractmethod
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        """Handle the request. Returns response tuple or None if not handled."""
        pass


class FunctionImportHandler(BaseHandler):
    """Handler for OData FunctionImport calls (Preparation/Activation/Discard)."""
    
    FUNCTION_IMPORTS = {
        "CheckRootPreparationAction",
        "CheckRootActivationAction", 
        "CheckRootDiscardAction",
    }
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        from . import state  # Import here to avoid circular dependency
        self.state = state
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        if ctx.method != "POST" or ctx.url_path not in self.FUNCTION_IMPORTS:
            return None
        
        def _guid_param(name: str) -> str:
            raw = (ctx.query_params.get(name) or "").strip()
            if raw.lower().startswith("guid'") and raw.endswith("'"):
                raw = raw[5:-1]
            return raw.strip("'")
        
        requesting_user = self.state._resolve_mock_user(ctx.headers)
        
        if ctx.url_path == "CheckRootPreparationAction":
            return self._handle_prepare(_guid_param, requesting_user, ctx.query_params)
        elif ctx.url_path == "CheckRootActivationAction":
            return self._handle_activate(_guid_param, requesting_user)
        else:
            return self._handle_discard(_guid_param, requesting_user)
    
    def _handle_prepare(
        self,
        guid_param: Callable[[str], str],
        requesting_user: str,
        query_params: Dict[str, Any]
    ) -> Tuple[int, Any, str]:
        """Handle CheckRootPreparationAction."""
        preserve_raw = (query_params.get("PreserveChanges") or "").strip().lower()
        preserve_changes = preserve_raw != "false"
        
        result_row = draft_service._draft_prepare(
            guid_param("ActiveUUID"),
            preserve_changes=preserve_changes,
            requesting_user=requesting_user,
            draft_uuid_hint=guid_param("DraftUUID"),
        )
        
        return self._process_draft_result(result_row, "CheckRootPreparationAction")
    
    def _handle_activate(
        self,
        guid_param: Callable[[str], str],
        requesting_user: str
    ) -> Tuple[int, Any, str]:
        """Handle CheckRootActivationAction."""
        draft_uuid = guid_param("DraftUUID")
        
        # Re-check ownership on Activate
        locking_row = draft_service._draft_owner_mismatch(draft_uuid, requesting_user)
        if locking_row is not None:
            return self._draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
        
        result_row = draft_service._draft_activate(draft_uuid)

        if result_row is None:
            return ResponseBuilder.not_found("CheckRootActivationAction: entity not found")

        if isinstance(result_row, tuple) and result_row[0] == "validation_error":
            missing_fields = result_row[1]
            missing_children = result_row[2]
            root_id_for_targets = state.draft_store["CheckRoots"][draft_uuid]["RootId"]
            return ResponseBuilder.validation_error(missing_fields, missing_children, root_id_for_targets)
        
        if isinstance(result_row, tuple) and result_row[0] == "foreign_lock":
            locked_draft = result_row[1]
            return self._draft_locked_response(locked_draft.get("InProcessByUser", config.MOCK_USER))
        
        state._set_sap_message("Черновик сохранён", code="DRAFT_ACTIVATED")
        return ResponseBuilder.ok(result_row)
    
    def _handle_discard(
        self,
        guid_param: Callable[[str], str],
        requesting_user: str
    ) -> Tuple[int, Any, str]:
        """Handle CheckRootDiscardAction."""
        draft_uuid = guid_param("DraftUUID")
        
        # Re-check ownership on Discard
        locking_row = draft_service._draft_owner_mismatch(draft_uuid, requesting_user)
        if locking_row is not None:
            return self._draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
        
        result_row = draft_service._draft_discard(draft_uuid)
        
        if result_row == "discarded-create-only":
            return ResponseBuilder.no_content()
        
        return self._process_draft_result(result_row, "CheckRootDiscardAction")
    
    def _process_draft_result(
        self,
        result_row: Any,
        operation: str
    ) -> Tuple[int, Any, str]:
        """Process common draft operation results."""
        if result_row is None:
            return ResponseBuilder.not_found(f"{operation}: entity not found")
        
        if result_row == "discarded-create-only":
            return ResponseBuilder.no_content()
        
        if isinstance(result_row, tuple) and result_row[0] == "foreign_lock":
            locked_draft = result_row[1]
            return self._draft_locked_response(locked_draft.get("InProcessByUser", config.MOCK_USER))
        
        return ResponseBuilder.ok(result_row)
    
    def _draft_locked_response(self, owner: str) -> Tuple[int, Dict, str]:
        """Format draft locked error response."""
        return ResponseBuilder.conflict(
            "DRAFT_LOCKED",
            f"Черновик уже редактируется пользователем: {state._mock_user_display_name(owner)}"
        )


class ServiceRootHandler(BaseHandler):
    """Handler for service root operations ($metadata, EntitySets list)."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        if ctx.method == "GET" and ctx.url_path == "":
            entity_sets = list(config.REFERENCE_DATA.keys()) + list(state.store.keys())
            return ResponseBuilder.ok({"EntitySets": entity_sets})
        
        return None


class CountHandler(BaseHandler):
    """Handler for $count requests."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        entity_set = Patterns.parse_count_endpoint(ctx.url_path)
        if ctx.method == "GET" and entity_set:
            data, _ = query._resolve_source(entity_set)
            if data is None:
                return ResponseBuilder.not_found(f"Unknown set: {entity_set}")
            
            data = resolvers.apply_filter(
                data,
                ctx.query_params.get("$filter"),
                ctx.query_params.get("search")
            )
            return (HttpStatus.OK, str(len(data)), "text/plain")
        
        return None


class ListHandler(BaseHandler):
    """Handler for entity list GET requests."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        entity_set = Patterns.parse_entity_set(ctx.url_path)
        if ctx.method == "GET" and entity_set:
            if entity_set in config.REFERENCE_DATA or entity_set in state.store:
                return query._list_response(
                    entity_set,
                    ctx.query_params,
                    state._resolve_mock_user(ctx.headers)
                )
        
        return None


class NavigationHandler(BaseHandler):
    """Handler for navigation property requests (to_Basic, to_Checks, etc.)."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        if ctx.method != "GET":
            return None
        
        # Try DraftAdministrativeData navigation
        admin_m = Patterns.NAV_DRAFT_ADMIN_RE.match(ctx.url_path)
        if admin_m:
            return self._handle_draft_admin(admin_m, ctx.headers)
        
        # Try SiblingEntity navigation
        sibling_m = Patterns.NAV_SIBLING_RE.match(ctx.url_path)
        if sibling_m:
            return self._handle_sibling(sibling_m)
        
        # Try collection navigations (to_Basic, to_Checks, to_Barriers)
        nav_m = Patterns.NAV_COLLECTION_RE.match(ctx.url_path)
        if nav_m:
            return self._handle_collection(nav_m, ctx.query_params)
        
        return None
    
    def _handle_draft_admin(
        self,
        match: re.Match,
        headers: Dict[str, str]
    ) -> Tuple[int, Any, str]:
        """Handle DraftAdministrativeData navigation."""
        active_uuid, draft_uuid = match.group(1), match.group(2)
        
        if draft_uuid == ZERO_GUID:
            # Lookup draft by active UUID
            draft_uuid, _draft_row = state._find_draft_by_active_uuid(active_uuid)
            if draft_uuid is None:
                return ResponseBuilder.not_found()
        
        admin_row = resolvers._draft_admin_data_row(
            draft_uuid,
            state._resolve_mock_user(headers)
        )
        
        if admin_row is None:
            return ResponseBuilder.not_found()
        
        wrapped = resolvers.wrap_entity(
            "DraftAdministrativeData",
            config.ENTITY_TYPES["DraftAdministrativeData"],
            admin_row,
            {"DraftUUID": draft_uuid}
        )
        
        return ResponseBuilder.ok(wrapped)
    
    def _handle_sibling(self, match: re.Match) -> Tuple[int, Any, str]:
        """Handle SiblingEntity navigation."""
        active_uuid, draft_uuid = match.group(1), match.group(2)
        
        sibling_row = resolvers._sibling_row(active_uuid, draft_uuid)
        if sibling_row is None:
            return ResponseBuilder.not_found()
        
        return ResponseBuilder.ok(
            resolvers.wrap_entity(
                "CheckRoots",
                config.ENTITY_TYPES["CheckRoots"],
                sibling_row,
                None
            )
        )
    
    def _handle_collection(
        self,
        match: re.Match,
        query_params: Dict[str, Any]
    ) -> Tuple[int, Any, str]:
        """Handle collection navigations (to_Basic, to_Checks, to_Barriers)."""
        from .dispatch import _nav_root_id
        
        root_id = _nav_root_id(match)
        instance_tag = match.group(2)
        nav = match.group(3)
        
        if nav == "to_Basic":
            return self._handle_basic(root_id)
        
        child_set = "CheckItems" if nav == "to_Checks" else "Barriers"
        id_prop = "ItemId" if nav == "to_Checks" else "BarrierId"
        
        rows = [
            (k, v) for k, v in state.store[child_set].items()
            if k[0] == root_id and v.get("DraftUUID", ZERO_GUID) == instance_tag
        ]
        
        raw_rows = [v for _k, v in rows]
        raw_rows = resolvers.apply_filter(
            raw_rows,
            query_params.get("$filter"),
            query_params.get("search")
        )
        raw_rows = resolvers.apply_orderby(raw_rows, query_params.get("$orderby"))
        
        skip = query._parse_int_param(query_params, "$skip", 0)
        top = query._parse_int_param(query_params, "$top", len(raw_rows))
        page = raw_rows[skip:skip + top]
        
        page = [
            resolvers.wrap_entity(
                child_set,
                config.ENTITY_TYPES[child_set],
                r,
                {"RootId": root_id, id_prop: r[id_prop]}
            )
            for r in page
        ]
        
        return ResponseBuilder.ok({
            "results": page,
            "__count": str(len(raw_rows))
        })
    
    def _handle_basic(self, root_id: str) -> Tuple[int, Any, str]:
        """Handle to_Basic navigation."""
        basic = state.store["CheckBasics"].get(root_id)
        if not basic:
            return ResponseBuilder.not_found()
        
        return ResponseBuilder.ok(
            resolvers.wrap_entity(
                "CheckBasics",
                config.ENTITY_TYPES["CheckBasics"],
                basic,
                {"RootId": root_id}
            )
        )


class SingleEntityHandler(BaseHandler):
    """Handler for single entity GET requests by key."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        from .dispatch import _parse_key
        
        if ctx.method != "GET":
            return None
        
        set_name, key_parts = _parse_key(ctx.url_path)
        if set_name:
            return query._single_response(
                set_name,
                key_parts,
                ctx.query_params,
                state._resolve_mock_user(ctx.headers)
            )
        
        return None


class CreateRootHandler(BaseHandler):
    """Handler for POST CheckRoots (create new draft root)."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        if ctx.method != "POST" or ctx.url_path != "CheckRoots":
            return None
        
        body = ctx.body or {}
        to_basic = body.pop("to_Basic", None) or {}
        to_checks = body.pop("to_Checks", None)
        to_barriers = body.pop("to_Barriers", None)
        body.pop("__metadata", None)
        
        # Merge proxy fields from root into to_Basic
        root_fields, basic_proxy_fields = query._split_root_body(body)
        to_basic = dict(to_basic)
        to_basic.update(basic_proxy_fields)

        # [Fix, use-case pass #5] Was: to_basic.setdefault("LocationKey",
        # "...440101"  # Площадка №1) - silently pre-filled EVERY new record
        # with the same hardcoded location before the inspector ever touched
        # the form. LocationName carries Common.Required=true (check-root.xml)
        # specifically so RequiredFieldsRule.js blocks Save on an unpicked
        # location - the default defeated that gate for the one field it
        # existed to guard, and risked a real inspection silently getting
        # attributed to "Площадка №1" if nobody noticed and changed it. No
        # default here now - LocationKey/LocationName simply stay unset until
        # the inspector picks one via F4, same as every other required field.
        
        root_id = odata_format.generate_uuid()
        now = odata_format.odata_date()
        
        root = dict(root_fields)
        root["RootId"] = root_id
        root["DocId"] = state.draw_doc_id()
        root.setdefault("Status", "OK")
        root.setdefault("ThisIsIntegrationData", False)
        root["CreatedBy"] = "MOCK_USER"
        root["CreatedAt"] = now
        root["LastChangedAt"] = now
        
        # Draft-specific fields
        root["ActiveUUID"] = ZERO_GUID
        root["DraftUUID"] = root_id
        root["IsActiveEntity"] = False
        root["_draft_created_at"] = now
        
        state.draft_store["CheckRoots"][root_id] = root
        
        crud._create_check_basic(root_id, to_basic, now)
        
        if to_checks and "results" in to_checks:
            for item_body in to_checks["results"]:
                crud._create_check_item(root_id, item_body, now, draft_uuid=root_id)
        
        if to_barriers and "results" in to_barriers:
            for barrier_body in to_barriers["results"]:
                crud._create_barrier(root_id, barrier_body, now, draft_uuid=root_id)
        
        response_row = resolvers.wrap_entity(
            "CheckRoots",
            config.ENTITY_TYPES["CheckRoots"],
            root,
            None
        )
        
        return ResponseBuilder.created(response_row)


class CreateChildHandler(BaseHandler):
    """Handler for POST to navigation properties (create child entities)."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        if ctx.method != "POST":
            return None
        
        nav_m = Patterns.NAV_CREATE_RE.match(ctx.url_path)
        if not nav_m:
            return None
        
        from .dispatch import _nav_root_id
        
        root_id = _nav_root_id(nav_m)
        instance_tag = nav_m.group(2)
        nav = nav_m.group(3)
        
        # Check if root exists
        if root_id not in state.store["CheckRoots"] and root_id not in state.draft_store["CheckRoots"]:
            return ResponseBuilder.not_found()
        
        now = odata_format.odata_date()
        body = dict(ctx.body or {})
        body.pop("__metadata", None)
        
        if nav == "to_Checks":
            item_id = crud._create_check_item(root_id, body, now, draft_uuid=instance_tag)
            row = state.store["CheckItems"][(root_id, item_id)]
            return ResponseBuilder.created(
                resolvers.wrap_entity(
                    "CheckItems",
                    config.ENTITY_TYPES["CheckItems"],
                    row,
                    {"RootId": root_id, "ItemId": item_id}
                )
            )
        
        barrier_id = crud._create_barrier(root_id, body, now, draft_uuid=instance_tag)
        row = state.store["Barriers"][(root_id, barrier_id)]
        return ResponseBuilder.created(
            resolvers.wrap_entity(
                "Barriers",
                config.ENTITY_TYPES["Barriers"],
                row,
                {"RootId": root_id, "BarrierId": barrier_id}
            )
        )


class UpdateHandler(BaseHandler):
    """Handler for PUT/MERGE/PATCH requests."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        from .dispatch import _parse_key
        
        if ctx.method not in ("PUT", "MERGE", "PATCH"):
            return None
        
        set_name, key_parts = _parse_key(ctx.url_path)
        if not set_name or set_name not in state.store:
            return None
        
        row = query._find_by_key(set_name, key_parts)
        if not row:
            return ResponseBuilder.not_found()
        
        if query._precondition_failed(set_name, key_parts, ctx.headers):
            return ResponseBuilder.precondition_failed()
        
        body = dict(ctx.body or {})
        body.pop("__metadata", None)
        body = serve_config.strip_readonly_fields(set_name, body)
        
        if set_name == "CheckRoots":
            return self._update_root(row, key_parts, body, ctx.headers)
        
        return self._update_entity(set_name, row, key_parts, body)
    
    def _update_root(
        self,
        row: Dict,
        key_parts: Dict,
        body: Dict,
        headers: Dict[str, str]
    ) -> Tuple[int, Any, str]:
        """Update CheckRoots entity (with draft awareness)."""
        root_id = row["RootId"]
        is_draft_addressed = key_parts.get("DraftUUID", ZERO_GUID) != ZERO_GUID
        
        # Re-check ownership for draft-addressed updates
        if is_draft_addressed:
            locking_row = draft_service._draft_owner_mismatch(
                key_parts["DraftUUID"],
                state._resolve_mock_user(headers)
            )
            if locking_row is not None:
                return ResponseBuilder.conflict(
                    "DRAFT_LOCKED",
                    f"Черновик уже редактируется пользователем: {state._mock_user_display_name(locking_row.get('InProcessByUser', config.MOCK_USER))}"
                )
        
        target_dict = state.draft_store["CheckRoots"] if is_draft_addressed else state.store["CheckRoots"]
        store_key = key_parts["DraftUUID"] if is_draft_addressed else key_parts["ActiveUUID"]
        
        # Handle Basic proxy fields
        root_fields, basic_proxy_fields = query._split_root_body(body)
        basic_proxy_fields = serve_config.strip_readonly_fields("CheckBasics", basic_proxy_fields)
        
        if basic_proxy_fields:
            basic_row = state.store["CheckBasics"].get(root_id, {"RootId": root_id})
            updated_basic = dict(basic_row)
            updated_basic.update(basic_proxy_fields)
            updated_basic["LastChangedAt"] = odata_format.odata_date()
            state.store["CheckBasics"][root_id] = updated_basic
        
        body = root_fields
        updated = dict(row)
        updated.update(body)
        updated["LastChangedAt"] = odata_format.odata_date()
        
        target_dict[store_key] = updated
        new_etag = odata_format._root_etag_string(
            root_id,
            root_override=updated if is_draft_addressed else None
        )
        state._set_pending_etag(f'W/"{new_etag}"')
        
        return ResponseBuilder.no_content()
    
    def _update_entity(
        self,
        set_name: str,
        row: Dict,
        key_parts: Dict,
        body: Dict
    ) -> Tuple[int, Any, str]:
        """Update non-root entity."""
        updated = dict(row)
        updated.update(body)
        updated["LastChangedAt"] = odata_format.odata_date()
        
        props = config.KEY_PROP_TUPLES[set_name]
        store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
        
        state.store[set_name][store_key] = updated
        state._set_pending_etag(f'W/"{updated["LastChangedAt"]}"')
        
        return ResponseBuilder.no_content()


class DeleteHandler(BaseHandler):
    """Handler for DELETE requests."""
    
    def __init__(self, container: Optional['ServiceContainer'] = None):
        """Initialize with optional dependency injection container."""
        self.container = container
    
    def handle(self, ctx: RequestContext) -> Optional[Tuple[int, Any, str]]:
        from .dispatch import _parse_key
        
        if ctx.method != "DELETE":
            return None
        
        set_name, key_parts = _parse_key(ctx.url_path)
        if not set_name or set_name not in state.store:
            return None
        
        row = query._find_by_key(set_name, key_parts)
        if not row:
            return ResponseBuilder.not_found()
        
        if query._precondition_failed(set_name, key_parts, ctx.headers):
            return ResponseBuilder.precondition_failed()
        
        if set_name == "CheckRoots":
            return self._delete_root(row, key_parts, ctx.headers)

        return self._delete_child(set_name, row, key_parts)

    def _delete_root(
        self,
        row: Dict,
        key_parts: Dict,
        headers: Optional[Dict] = None
    ) -> Tuple[int, Any, str]:
        """Delete CheckRoots entity (with draft awareness)."""
        root_id = row["RootId"]

        if key_parts.get("DraftUUID", ZERO_GUID) != ZERO_GUID:
            # Draft-addressed DELETE = discard
            # [Fix] Was hardcoded state._resolve_mock_user({}) - an empty
            # dict, ignoring whatever X-Mock-User header the actual request
            # carried, so this always re-checked ownership against the
            # DEFAULT identity instead of the real requester (see
            # HandlersRefactorRegressionTests for the live-repro).
            locking_row = draft_service._draft_owner_mismatch(
                key_parts["DraftUUID"],
                state._resolve_mock_user(headers)
            )
            if locking_row is not None:
                return ResponseBuilder.conflict(
                    "DRAFT_LOCKED",
                    f"Черновик уже редактируется пользователем: {state._mock_user_display_name(locking_row.get('InProcessByUser', config.MOCK_USER))}"
                )
            
            draft_row = state.draft_store["CheckRoots"].pop(key_parts["DraftUUID"], None)
            if draft_row is not None:
                if root_id in state.store["CheckRoots"]:
                    draft_service._restore_children(
                        root_id,
                        draft_row.get("_child_snapshot")
                    )
                    draft_service._purge_tagged_children(root_id, key_parts["DraftUUID"])
                else:
                    draft_service._purge_children(root_id)
        else:
            # Active entity DELETE
            state.store["CheckRoots"].pop(key_parts["ActiveUUID"], None)
            state.store["CheckBasics"].pop(root_id, None)
            
            # Remove active-tagged children
            for k in [
                k for k, v in state.store["CheckItems"].items()
                if k[0] == root_id and v.get("DraftUUID", ZERO_GUID) == ZERO_GUID
            ]:
                del state.store["CheckItems"][k]
            
            for k in [
                k for k, v in state.store["Barriers"].items()
                if k[0] == root_id and v.get("DraftUUID", ZERO_GUID) == ZERO_GUID
            ]:
                del state.store["Barriers"][k]
            
            # Remove orphaned edit-draft
            orphan_draft_uuid, _orphan_draft_row = state._find_draft_by_active_uuid(root_id)
            if orphan_draft_uuid:
                state.draft_store["CheckRoots"].pop(orphan_draft_uuid, None)
                draft_service._purge_tagged_children(root_id, orphan_draft_uuid)
        
        return ResponseBuilder.no_content()
    
    def _delete_child(
        self,
        set_name: str,
        row: Dict,
        key_parts: Dict
    ) -> Tuple[int, Any, str]:
        """Delete child entity (CheckItems/Barriers)."""
        props = config.KEY_PROP_TUPLES[set_name]
        store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
        
        deleted_row = state.store[set_name][store_key]
        del state.store[set_name][store_key]
        
        if set_name in ("CheckItems", "Barriers"):
            child_instance_tag = deleted_row.get("DraftUUID", ZERO_GUID)
            if child_instance_tag == ZERO_GUID:
                root = state.store["CheckRoots"].get(store_key[0])
                if root:
                    root["LastChangedAt"] = odata_format.odata_date()
            else:
                draft_row = state.draft_store["CheckRoots"].get(child_instance_tag)
                if draft_row:
                    draft_row["LastChangedAt"] = odata_format.odata_date()
        
        return ResponseBuilder.no_content()


class RequestDispatcher:
    """Main dispatcher that routes requests to appropriate handlers."""
    
    def __init__(self):
        self.handlers: List[BaseHandler] = [
            FunctionImportHandler(),
            ServiceRootHandler(),
            CountHandler(),
            ListHandler(),
            NavigationHandler(),
            SingleEntityHandler(),
            CreateRootHandler(),
            CreateChildHandler(),
            UpdateHandler(),
            DeleteHandler(),
        ]
    
    def dispatch(
        self,
        method: str,
        rel_url: str,
        body: Optional[Dict] = None,
        headers: Optional[Dict] = None
    ) -> Tuple[int, Any, str]:
        """Dispatch request to appropriate handler."""
        ctx = RequestContext.from_raw(method, rel_url, body, headers)
        
        for handler in self.handlers:
            result = handler.handle(ctx)
            if result is not None:
                return result
        
        # No handler matched - unknown route.
        # [Fix] Was ResponseBuilder.bad_request (400, with an added 'code'/
        # 'lang' shape) - the pre-refactor original returned a plain 404
        # here, matching the SAP Gateway convention for a nonexistent OData
        # resource path. Restored to 404 for behavioral parity; nothing in
        # the client (sap.ui.model.odata.v2.ODataModel) distinguishes
        # 400-vs-404 specially for a route that should never be hit by the
        # real UI anyway, but tests should still pin the intended status so
        # it can't silently drift again.
        return ResponseBuilder.not_found(f"Unknown route: {method} {rel_url}")


# Legacy function for backward compatibility
def dispatch_request(
    method: str,
    rel_url: str,
    body: Optional[Dict] = None,
    headers: Optional[Dict] = None
) -> Tuple[int, Any, str]:
    """Legacy dispatch function - delegates to RequestDispatcher."""
    dispatcher = RequestDispatcher()
    return dispatcher.dispatch(method, rel_url, body, headers)
