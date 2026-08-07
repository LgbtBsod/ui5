"""URL/method routing: parses the request path into (entity set, key) or a
FunctionImport call, and dispatches to query.py/draft_service.py/crud.py.
This is the one module that knows about every route shape - everything
downstream of it is oblivious to HTTP.
"""
import re
from urllib.parse import parse_qs, unquote

import serve_config

from . import config
from . import state
from . import odata_format
from . import resolvers
from . import query
from . import draft_service
from . import crud

SINGLE_KEY_RE = re.compile(r"^(\w+)\('([^']*)'\)$")
COMPOSITE_KEY_RE = re.compile(r"^(\w+)\(RootId='([^']*)',(ItemId|BarrierId)='([^']*)'\)$")
# [Фаза 5, NW 7.50/pre-S4 1610] CheckRoots — составной ActiveUUID/DraftUUID
# ключ (Edm.Guid, литерал guid'...'), а НЕ IsActiveEntity — см.
# localService/metadata.xml, комментарий у EntityType CheckRoot.
CHECKROOT_KEY_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)$")
# [Фаза 4] Композиция (CheckBasics/CheckItems/Barriers) не участвует в
# draft-режиме (см. state.draft_store) — читается/пишется от RootId одинаково
# вне зависимости от того, активен корень или ещё черновик из CREATE.
NAV_COLLECTION_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Basic|to_Checks|to_Barriers)$")
NAV_CREATE_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Checks|to_Barriers)$")
# [Fix] Single-valued (0..1) nav to DraftAdministrativeData - see
# resolvers._draft_admin_data_row. Deliberately separate from
# NAV_COLLECTION_RE (that one always wraps results in a {"results": [...]}
# collection envelope, which is wrong for a single-entity nav target).
NAV_DRAFT_ADMIN_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/DraftAdministrativeData$")
# [Fix] Single-valued (0..1) nav to SiblingEntity - the other half of the
# active/draft pair (see metadata.xml Association CheckRoot_SiblingEntity).
# Discovered missing live: the standard EditState SmartFilterBar quick
# filter references "SiblingEntity/IsActiveEntity" in its generated $filter,
# which needs a real NavigationProperty of this name to type-check.
NAV_SIBLING_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/SiblingEntity$")


def _nav_root_id(m):
    """Resolves the effective RootId from a NAV_COLLECTION_RE/NAV_CREATE_RE
    match's (ActiveUUID, DraftUUID) pair — the active row's own id when
    addressed via ActiveUUID (DraftUUID = ZERO_GUID), otherwise the owning
    draft row's RootId (create-drafts carry their own uuid as RootId)."""
    active_uuid, draft_uuid = m.group(1), m.group(2)
    if draft_uuid == config.ZERO_GUID:
        return active_uuid
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    return draft_row["RootId"] if draft_row else active_uuid


def _parse_key(url_path):
    """Returns (set_name, key_parts_dict) or (None, None)."""
    m = CHECKROOT_KEY_RE.match(url_path)
    if m:
        return "CheckRoots", {"ActiveUUID": m.group(1), "DraftUUID": m.group(2)}
    m = COMPOSITE_KEY_RE.match(url_path)
    if m:
        set_name, root_id, id_prop, id_val = m.groups()
        return set_name, {"RootId": unquote(root_id), id_prop: unquote(id_val)}
    m = SINGLE_KEY_RE.match(url_path)
    if m:
        set_name, key = m.groups()
        props = config.KEY_PROP_TUPLES.get(set_name) or (serve_config.REFERENCE_KEY_PROPS.get(set_name),)
        if props[0] is None:
            return None, None
        return set_name, {props[0]: unquote(key)}
    return None, None


def _format_error_response(code, message_value, severity="error", http_status=400):
    """Format error response in SAP Gateway OData v2 format.
    
    Args:
        code: Error code string (e.g., 'VALIDATION_ERROR', 'DRAFT_LOCKED')
        message_value: Human-readable message text
        severity: Error severity ('error', 'warning', 'info')
        http_status: HTTP status code (default 400)
    
    Returns:
        Tuple of (http_status, error_dict, content_type)
        
    SAP Gateway OData v2 error format follows the convention:
    {
        \"error\": {
            \"code\": \"ERROR_CODE\",
            \"message\": {\"value\": \"Human readable message\", \"lang\": \"en\"},
            \"innererror\": {
                \"errordetails\": [
                    {
                        \"code\": \"DETAIL_CODE\",
                        \"message\": {\"value\": \"Detail message\"},
                        \"severity\": \"error\",
                        \"target\": \"PropertyName\"
                    }
                ]
            }
        }
    }
    """
    return (http_status, {
        "error": {
            "code": code,
            "message": {"value": message_value, "lang": "ru"},
        }
    }, "application/json")


def _draft_locked_response(owner):
    """Shared 409 DRAFT_LOCKED shape - used both by the one-time
    CheckRootPreparationAction foreign-lock branch and by
    draft_service._draft_owner_mismatch's re-check on every later write to
    an already-open draft (Activate/Discard/PATCH/MERGE/PUT/DELETE addressed
    directly by the draft's own key)."""
    return _format_error_response(
        code="DRAFT_LOCKED",
        message_value="Черновик уже редактируется пользователем: %s" % state._mock_user_display_name(owner),
        http_status=409
    )


def dispatch_request(method, rel_url, body=None, headers=None):
    headers = headers or {}
    parts = rel_url.split("?", 1)
    raw_path = parts[0]
    query_params = {}
    if len(parts) > 1:
        for k, v in parse_qs(parts[1]).items():
            query_params[k] = v[0] if len(v) == 1 else v

    url_path = raw_path.lstrip("/")

    # [Фаза 5, NW 7.50/pre-S4 1610] FunctionImport-вызовы OData V2
    # (sap:action-for) — ServiceRoot/FunctionImportName?Param=guid'X', не
    # nav-путь off ключа (это V4/RAP-конвенция). Обрабатываются до общей
    # key-based маршрутизации, т.к. url_path здесь — просто имя FunctionImport,
    # не EntitySet(key). Preparation адресует АКТИВНУЮ запись (ActiveUUID) —
    # для неё готовится черновик; Activation/Discard адресуют КОНКРЕТНЫЙ
    # черновик (DraftUUID) — см. localService/metadata.xml.
    if method == "POST" and url_path in (
        "CheckRootPreparationAction", "CheckRootActivationAction", "CheckRootDiscardAction"
    ):
        def _guid_param(name):
            raw = (query_params.get(name) or "").strip()
            if raw.lower().startswith("guid'") and raw.endswith("'"):
                raw = raw[5:-1]
            return raw.strip("'")

        requesting_user = state._resolve_mock_user(headers)
        if url_path == "CheckRootPreparationAction":
            # [Fix] PreserveChanges (see metadata.xml FunctionImport) - only
            # meaningful when an edit-draft already exists; explicit "false"
            # discards it and starts fresh from the current active row
            # instead of silently resuming stale in-progress edits.
            preserve_raw = (query_params.get("PreserveChanges") or "").strip().lower()
            preserve_changes = preserve_raw != "false"
            result_row = draft_service._draft_prepare(
                _guid_param("ActiveUUID"), preserve_changes=preserve_changes,
                requesting_user=requesting_user, draft_uuid_hint=_guid_param("DraftUUID"),
            )
        elif url_path == "CheckRootActivationAction":
            draft_uuid = _guid_param("DraftUUID")
            # [Fix, exhaustive-sweep pass] see draft_service._draft_owner_mismatch -
            # Activate/Discard addressed directly by an existing draft's own
            # key were never re-checked against the CURRENT requesting user,
            # unlike the one-time Prepare call.
            locking_row = draft_service._draft_owner_mismatch(draft_uuid, requesting_user)
            if locking_row is not None:
                return _draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
            result_row = draft_service._draft_activate(draft_uuid)
        else:
            draft_uuid = _guid_param("DraftUUID")
            locking_row = draft_service._draft_owner_mismatch(draft_uuid, requesting_user)
            if locking_row is not None:
                return _draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
            result_row = draft_service._draft_discard(draft_uuid)

        if result_row is None:
            return (404, {"error": {"message": {"value": "%s: entity not found" % url_path}}}, "application/json")
        # [Фаза 4] Discard черновика из CREATE (никогда не был активным) не
        # возвращает сущность — отменять нечего, кроме самого черновика.
        if result_row == "discarded-create-only":
            return (204, None, "application/json")
        # [Fix] draft_service._draft_activate's required-field gate (see
        # draft_service._missing_required_fields) - Activate on an object
        # with a blank required field is rejected instead of silently
        # succeeding; the draft itself is left untouched so the user can fix
        # the field and retry.
        if isinstance(result_row, tuple) and result_row[0] == "validation_error":
            missing_fields = result_row[1]
            missing_children = result_row[2]
            # [Fix] Field-level (target-bound) messages, alongside the
            # existing combined-text one. Verified by reading the actual
            # sap/ui/core ODataMessageParser source: _parseBodyJSON pushes
            # BOTH the top-level error AND every entry of
            # error.innererror.errordetails[] through the same
            # _createMessage (which reads `.target`), so a single response
            # can carry both an overall MessageBox-style summary (still the
            # top-level `message`, unchanged for the existing behavior/tests)
            # and N field-specific messages a SmartField can bind against to
            # show its own red-underline - each `target` is just the bare
            # property name on CheckRoot (the request addresses the entity
            # directly, no navigation prefix needed).
            #
            # [Fix, exhaustive-sweep pass] missing_children entries need a
            # NAV-QUALIFIED target instead - confirmed by reading the real
            # ODataMessageParser._createTarget source: for a non-absolute
            # target it string-concatenates the FunctionImport's own
            # resolved entity path ("CheckRoots(ActiveUUID=..,DraftUUID=..)")
            # with whatever relative target we supply, then runs the result
            # through ODataModel.resolve()/ODataMetadata._calculateCanonicalPath
            # (pure metadata/string structural resolution, no dependency on
            # the child already being loaded) - so
            # "to_Checks(RootId='X',ItemId='Y')/Result" resolves to exactly
            # the canonical model path the child's own SmartTable row is
            # bound against, red-underlining that row's cell instead of the
            # (nonexistent, on this FunctionImport's own entity) bare field.
            all_labels = [label for _f, label in missing_fields] + [c["label"] for c in missing_children]
            # The draft is left untouched on validation failure (see
            # draft_service._draft_activate), so its own row - and RootId -
            # is still resolvable here for building child targets below.
            root_id_for_targets = state.draft_store["CheckRoots"][draft_uuid]["RootId"]
            return (400, {
                "error": {
                    "code": "VALIDATION_ERROR",
                    "message": {"value": "Не заполнены обязательные поля: " + ", ".join(all_labels)},
                    "innererror": {
                        "errordetails": [
                            {
                                "code": "VALIDATION_ERROR",
                                "message": {"value": "%s: обязательное поле" % label},
                                "severity": "error",
                                "target": field,
                            }
                            for field, label in missing_fields
                        ] + [
                            {
                                "code": "VALIDATION_ERROR",
                                "message": {"value": "%s: обязательное поле" % c["label"]},
                                "severity": "error",
                                "target": "%s(RootId='%s',%s='%s')/%s" % (c["nav"], root_id_for_targets, c["id_prop"], c["item_id"], c["field"]),
                            }
                            for c in missing_children
                        ]
                    }
                }
            }, "application/json")
        # [Fix] draft_service._draft_prepare's foreign-lock gate - matches
        # the real sap.ui.generic.app convention (fnCallEdit's error handler
        # specifically checks statusCode==="409" before trying the friendly
        # "locked by X" recovery flow - see CRUDManager.checkForForeignUserLock).
        # The draft itself is untouched; the framework will follow up with its
        # own $expand=DraftAdministrativeData read (now correctly resolvable
        # from the active row - see resolvers.apply_expand) to get the
        # owner's name for the message it actually shows the user, so the
        # body here is mostly a sane fallback for direct/manual inspection.
        if isinstance(result_row, tuple) and result_row[0] == "foreign_lock":
            locked_draft = result_row[1]
            return _draft_locked_response(locked_draft.get("InProcessByUser", config.MOCK_USER))
        if url_path == "CheckRootActivationAction":
            state._set_sap_message("Черновик сохранён", code="DRAFT_ACTIVATED")
        return (200, {"d": result_row}, "application/json")

    if method == "GET" and url_path == "":
        entity_sets = list(config.REFERENCE_DATA.keys()) + list(state.store.keys())
        return (200, {"d": {"EntitySets": entity_sets}}, "application/json")

    m = re.match(r'^(\w+)/\$count$', url_path)
    if method == "GET" and m:
        set_name = m.group(1)
        data, _ = query._resolve_source(set_name)
        if data is None:
            return (404, {"error": {"message": {"value": "Unknown set: %s" % set_name}}}, "application/json")
        data = resolvers.apply_filter(data, query_params.get("$filter"), query_params.get("search"))
        return (200, str(len(data)), "text/plain")

    m = re.match(r'^(\w+)$', url_path)
    if method == "GET" and m and (m.group(1) in config.REFERENCE_DATA or m.group(1) in state.store):
        return query._list_response(m.group(1), query_params, state._resolve_mock_user(headers))

    prefix = config.ODATA_PREFIX.lstrip("/")
    if url_path.startswith(prefix):
        url_path = url_path[len(prefix):].lstrip("/")

    if method == "GET":
        admin_m = NAV_DRAFT_ADMIN_RE.match(url_path)
        if admin_m:
            active_uuid, draft_uuid = admin_m.group(1), admin_m.group(2)
            if draft_uuid == config.ZERO_GUID:
                # [Fix] Active row's own DraftUUID field is always the
                # ZERO_GUID stub, but that doesn't mean there's no draft to
                # navigate to - mirrors NAV_SIBLING_RE/resolvers._sibling_row's
                # identical active->draft lookup-by-RootId fallback (and
                # resolvers.apply_expand's DraftAdministrativeData branch above).
                draft_uuid, _draft_row = state._find_draft_by_active_uuid(active_uuid)
                if draft_uuid is None:
                    return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            admin_row = resolvers._draft_admin_data_row(draft_uuid, state._resolve_mock_user(headers))
            if admin_row is None:
                return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            wrapped = resolvers.wrap_entity("DraftAdministrativeData", config.ENTITY_TYPES["DraftAdministrativeData"], admin_row, {"DraftUUID": draft_uuid})
            return (200, {"d": wrapped}, "application/json")

        sibling_m = NAV_SIBLING_RE.match(url_path)
        if sibling_m:
            active_uuid, draft_uuid = sibling_m.group(1), sibling_m.group(2)
            sibling_row = resolvers._sibling_row(active_uuid, draft_uuid)
            if sibling_row is None:
                return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            return (200, {"d": resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], sibling_row, None)}, "application/json")

        nav_m = NAV_COLLECTION_RE.match(url_path)
        if nav_m:
            root_id = _nav_root_id(nav_m)
            # [Fix] CheckItems/Barriers are real draft nodes now - reading
            # to_Checks/to_Barriers from a draft-addressed parent must only
            # see THAT draft's own tagged rows, not the active instance's
            # (and vice versa). CheckBasics stays root-scoped (unchanged).
            instance_tag = nav_m.group(2)
            nav = nav_m.group(3)
            if nav == "to_Basic":
                basic = state.store["CheckBasics"].get(root_id)
                if not basic:
                    return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
                return (200, {"d": resolvers.wrap_entity("CheckBasics", config.ENTITY_TYPES["CheckBasics"], basic, {"RootId": root_id})}, "application/json")
            child_set = "CheckItems" if nav == "to_Checks" else "Barriers"
            id_prop = "ItemId" if nav == "to_Checks" else "BarrierId"
            rows = [
                (k, v) for k, v in state.store[child_set].items()
                if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == instance_tag
            ]
            raw_rows = [v for _k, v in rows]
            raw_rows = resolvers.apply_filter(raw_rows, query_params.get("$filter"), query_params.get("search"))
            raw_rows = resolvers.apply_orderby(raw_rows, query_params.get("$orderby"))
            skip = query._parse_int_param(query_params, "$skip", 0)
            top = query._parse_int_param(query_params, "$top", len(raw_rows))
            page = raw_rows[skip:skip + top]
            page = [
                resolvers.wrap_entity(child_set, config.ENTITY_TYPES[child_set], r, {"RootId": root_id, id_prop: r[id_prop]})
                for r in page
            ]
            return (200, {"d": {"results": page, "__count": str(len(raw_rows))}}, "application/json")

        set_name, key_parts = _parse_key(url_path)
        if set_name:
            return query._single_response(set_name, key_parts, query_params, state._resolve_mock_user(headers))

    if method == "POST" and url_path == "CheckRoots":
        body = body or {}
        to_basic = body.pop("to_Basic", None) or {}
        to_checks = body.pop("to_Checks", None)
        to_barriers = body.pop("to_Barriers", None)
        body.pop("__metadata", None)

        # [Basic-proxy] Basic-поля могли прийти двумя путями: через nested
        # to_Basic (deep-insert, "официальный" OData-путь) ИЛИ прямо в теле
        # Root (если клиент писал через прокси-поля, см. config.BASIC_PROXY_FIELDS
        # выше) — оба варианта равнозначны, объединяем.
        root_fields, basic_proxy_fields = query._split_root_body(body)
        to_basic = dict(to_basic)
        to_basic.update(basic_proxy_fields)

        # [Fix] Required-field validation on plain create REMOVED - this
        # entity is draft-enabled (Common.DraftRoot), so POST /CheckRoots
        # creates an incomplete, in-progress DRAFT (IsActiveEntity=false),
        # exactly like the standard Fiori Elements "Create" flow: a blank
        # object the user fills in over several field edits before ever
        # saving. Rejecting that here with a 400 broke Create outright
        # (confirmed live: clicking "Создать" in the List Report always
        # failed with this same error, since the framework's initial
        # createEntry() payload has no field values yet). Required-field
        # enforcement correctly lives at Activation time only - see
        # config.REQUIRED_ROOT_FIELDS/draft_service._missing_required_fields
        # used by draft_service._draft_activate below, which is where the
        # real BOPF/CDS convention validates too (draft check -> CHECK
        # method fires on Activate, not on Create).
        #
        # [FIX] Дефолтные значения для базовых полей при создании через UI
        # (когда клиент не передал значения). Без этого transient-объект
        # не имеет полей в FieldGroup#General и форма пустая.
        to_basic.setdefault("LocationKey", "550e8400-e29b-41d4-a716-446655440101")  # Площадка №1

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

        # [Фаза 5, NW 7.50/pre-S4 1610] CheckRoots — draft-root: POST создаёт
        # ЧЕРНОВИК (в draft_store, БЕЗ активной записи в store), а не сразу
        # активную запись — как настоящий ручной draft на create (см.
        # localService/metadata.xml). Create-драфт ещё не имеет активного
        # двойника — ActiveUUID = ZERO_GUID; DraftUUID совпадает с RootId
        # (тем же generate_uuid(), новый отдельный id не нужен — см.
        # draft_service._draft_activate).
        # [Fix] CheckBasics остаётся root-scoped (proxy-поля на самом Root),
        # но CheckItems/Barriers теперь настоящие draft-узлы
        # (Common.DraftNode) - их строки тегируются DraftUUID=root_id (тем же
        # черновиком), а не "активной" таблицей напрямую;
        # draft_service._draft_activate перетегирует их на ZERO_GUID при
        # активации.
        root["ActiveUUID"] = config.ZERO_GUID
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

        response_row = resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], root, None)
        return (201, {"d": response_row}, "application/json")

    if method == "POST":
        nav_m = NAV_CREATE_RE.match(url_path)
        if nav_m:
            root_id = _nav_root_id(nav_m)
            # [Fix] CheckItems/Barriers are real draft nodes now - a new row
            # created via nav from a draft-addressed parent must be tagged
            # with THAT draft's uuid (only visible/editable inside this
            # draft session), not silently written straight into the active
            # instance's children.
            instance_tag = nav_m.group(2)
            nav = nav_m.group(3)
            # [Фаза 4] Тот же черновик-из-create гейт, что и у FunctionImport
            # роутинга — корень мог ещё не существовать в активном store.
            if root_id not in state.store["CheckRoots"] and root_id not in state.draft_store["CheckRoots"]:
                return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            now = odata_format.odata_date()
            body = dict(body or {})
            body.pop("__metadata", None)
            if nav == "to_Checks":
                item_id = crud._create_check_item(root_id, body, now, draft_uuid=instance_tag)
                row = state.store["CheckItems"][(root_id, item_id)]
                return (201, {"d": resolvers.wrap_entity("CheckItems", config.ENTITY_TYPES["CheckItems"], row, {"RootId": root_id, "ItemId": item_id})}, "application/json")
            barrier_id = crud._create_barrier(root_id, body, now, draft_uuid=instance_tag)
            row = state.store["Barriers"][(root_id, barrier_id)]
            return (201, {"d": resolvers.wrap_entity("Barriers", config.ENTITY_TYPES["Barriers"], row, {"RootId": root_id, "BarrierId": barrier_id})}, "application/json")

    if method in ("PUT", "MERGE", "PATCH"):
        set_name, key_parts = _parse_key(url_path)
        if set_name and set_name in state.store:
            row = query._find_by_key(set_name, key_parts)
            if not row:
                return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            if query._precondition_failed(set_name, key_parts, headers):
                return (412, {"error": {"message": {"value": "Precondition Failed"}}}, "application/json")
            body = dict(body or {})
            body.pop("__metadata", None)
            body = serve_config.strip_readonly_fields(set_name, body)

            if set_name == "CheckRoots":
                # [Фаза 5] Ключ — ActiveUUID/DraftUUID, не RootId; целевой
                # словарь и физический store_key зависят от того, черновик
                # адресован (DraftUUID != ZERO_GUID) или активная запись.
                root_id = row["RootId"]
                is_draft_addressed = key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID
                # [Fix, exhaustive-sweep pass] see draft_service._draft_owner_mismatch -
                # a raw PATCH/MERGE/PUT addressed directly by an existing
                # draft's own key never went through the one-time Prepare
                # gate at all.
                if is_draft_addressed:
                    locking_row = draft_service._draft_owner_mismatch(key_parts["DraftUUID"], state._resolve_mock_user(headers))
                    if locking_row is not None:
                        return _draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
                target_dict = state.draft_store["CheckRoots"] if is_draft_addressed else state.store["CheckRoots"]
                store_key = key_parts["DraftUUID"] if is_draft_addressed else key_parts["ActiveUUID"]

                # [Basic-proxy] см. config.BASIC_PROXY_FIELDS выше — часть
                # тела PATCH на Root маршрутизируется в CheckBasics
                # (CheckBasics сама draft-неосведомлена, см. state.draft_store
                # — пишем всегда в активную по RootId).
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
                new_etag = odata_format._root_etag_string(root_id, root_override=updated if is_draft_addressed else None)
                state._set_pending_etag('W/"%s"' % new_etag)
                return (204, None, "application/json")

            updated = dict(row)
            updated.update(body)
            updated["LastChangedAt"] = odata_format.odata_date()
            props = config.KEY_PROP_TUPLES[set_name]
            store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
            state.store[set_name][store_key] = updated
            state._set_pending_etag('W/"%s"' % updated["LastChangedAt"])
            return (204, None, "application/json")

    if method == "DELETE":
        set_name, key_parts = _parse_key(url_path)
        if set_name and set_name in state.store:
            row = query._find_by_key(set_name, key_parts)
            if not row:
                return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
            if query._precondition_failed(set_name, key_parts, headers):
                return (412, {"error": {"message": {"value": "Precondition Failed"}}}, "application/json")
            if set_name == "CheckRoots":
                # [Фаза 5] Черновик-адресованный DELETE отменяет только
                # черновик (как Discard); активную запись со всей композицией
                # DELETE удаляет только когда адресована сама активная запись.
                root_id = row["RootId"]
                if key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID:
                    # [Fix, exhaustive-sweep pass] see draft_service._draft_owner_mismatch -
                    # a raw DELETE on a draft-addressed key IS a discard (see
                    # the comment below), so it needs the exact same
                    # ownership re-check CheckRootDiscardAction now has.
                    locking_row = draft_service._draft_owner_mismatch(key_parts["DraftUUID"], state._resolve_mock_user(headers))
                    if locking_row is not None:
                        return _draft_locked_response(locking_row.get("InProcessByUser", config.MOCK_USER))
                    draft_row = state.draft_store["CheckRoots"].pop(key_parts["DraftUUID"], None)
                    # [Fix] Same rollback/cleanup semantics as
                    # CheckRootDiscardAction (draft_service._draft_discard) -
                    # a raw DELETE on a draft-addressed key IS a discard, and
                    # must not leave behind either uncommitted CheckBasics
                    # edits (edit-draft) or orphaned composition rows
                    # (create-draft). CheckItems/Barriers are real draft
                    # nodes - dropping this draft's own tagged rows is
                    # enough, the active-tagged ones were never touched.
                    if draft_row is not None:
                        if root_id in state.store["CheckRoots"]:
                            draft_service._restore_children(root_id, draft_row.get("_child_snapshot"))
                            draft_service._purge_tagged_children(root_id, key_parts["DraftUUID"])
                        else:
                            draft_service._purge_children(root_id)
                else:
                    state.store["CheckRoots"].pop(key_parts["ActiveUUID"], None)
                    state.store["CheckBasics"].pop(root_id, None)
                    # [Fix] CheckItems/Barriers are real draft nodes - only
                    # the active-tagged (ZERO_GUID) rows belong to this
                    # active object; any outstanding draft's own tagged rows
                    # are cleaned up right below via
                    # draft_service._purge_tagged_children.
                    for k in [
                        k for k, v in state.store["CheckItems"].items()
                        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID
                    ]:
                        del state.store["CheckItems"][k]
                    for k in [
                        k for k, v in state.store["Barriers"].items()
                        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID
                    ]:
                        del state.store["Barriers"][k]
                    # [Fix] Deleting the active row must also drop any
                    # outstanding edit-draft that still points at it -
                    # otherwise the orphaned draft_store entry keeps
                    # HasActiveEntity=true (a lie), CheckRootActivationAction
                    # can resurrect a ghost record from it, and its own
                    # tagged CheckItems/Barriers would leak forever.
                    orphan_draft_uuid, _orphan_draft_row = state._find_draft_by_active_uuid(root_id)
                    if orphan_draft_uuid:
                        state.draft_store["CheckRoots"].pop(orphan_draft_uuid, None)
                        draft_service._purge_tagged_children(root_id, orphan_draft_uuid)
                return (204, None, "application/json")

            props = config.KEY_PROP_TUPLES[set_name]
            store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
            deleted_row = state.store[set_name][store_key]
            del state.store[set_name][store_key]
            if set_name in ("CheckItems", "Barriers"):
                # [Fix] CheckItems/Barriers are real draft nodes - bump the
                # timestamp of whichever "instance" this row actually
                # belonged to (its own DraftUUID tag), not always the active
                # row. Deleting a row that only existed inside an in-progress
                # draft must never touch the active object's LastChangedAt/
                # ETag - that would spuriously invalidate every other
                # reader's cached active-side etag for a change that isn't
                # even visible to them yet.
                child_instance_tag = deleted_row.get("DraftUUID", config.ZERO_GUID)
                if child_instance_tag == config.ZERO_GUID:
                    root = state.store["CheckRoots"].get(store_key[0])
                    if root:
                        root["LastChangedAt"] = odata_format.odata_date()
                else:
                    draft_row = state.draft_store["CheckRoots"].get(child_instance_tag)
                    if draft_row:
                        draft_row["LastChangedAt"] = odata_format.odata_date()
            return (204, None, "application/json")

    return (404, {"error": {"message": {"value": "Unknown route: %s %s" % (method, rel_url)}}}, "application/json")
