"""Service document, $metadata, DictionaryItemSet, PersonVHSet, RuntimeSettingsSet routes."""
from fastapi import APIRouter, Depends, Query, Response
from sqlalchemy import asc
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import DictionaryItem, Person
from services.metadata_cache import get_metadata, metadata_refreshed_at_iso
from services.settings_service import SettingsService
from repo.settings_repo import SettingsRepo
from utils.filter_parser import FilterParser
from utils.filter_engine import parse_filter_to_predicate
from utils.filter_errors import FilterSyntaxError
from utils.odata import SERVICE_ROOT, odata_payload
from utils.odata_response import odata_collection, odata_entity
from utils.odata_query import parse_paging
from api.gateway_core import _err, _entity_metadata, _parse_odata_datetime
from api.gateway_serializers import (
    PERSON_VH_MAP, _dictionary_config_rows, _apply_orderby_rows,
    _person_full_name, _to_person_vh,
)

router = APIRouter(tags=["GatewayCanonical"])


@router.api_route(f"{SERVICE_ROOT}/", methods=["GET", "HEAD"])
def service_document():
    return odata_entity({"EntitySets": [
        "ChecklistSearchSet", "ChecklistRootSet", "ChecklistBasicInfoSet", "ChecklistCheckSet", "ChecklistBarrierSet",
        "DictionaryItemSet", "PersonVHSet", "LastChangeSet", "LockStatusSet", "ChecklistPermissionSet", "ChecklistCreatePermissionSet", "RuntimeSettingsSet",
        "CurrentUserSet", "SimpleAnalyticalSet", "WorkflowAnalyticsBreakdownSet", "AnalyticsRefreshStateSet", "AttachmentFolderSet", "AttachmentSet",
    ]})


@router.get(f"{SERVICE_ROOT}/$metadata")
def metadata():
    oResponse = Response(content=get_metadata(), media_type="application/xml; charset=utf-8")
    sRefreshedAt = metadata_refreshed_at_iso()
    if sRefreshedAt:
        oResponse.headers["X-Metadata-Refreshed-At"] = sRefreshedAt
    return oResponse


@router.get(f"{SERVICE_ROOT}/DictionaryItemSet")
def dictionary_item_set(filter: str | None = Query(None, alias="$filter"), orderby: str | None = Query(None, alias="$orderby"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    def _to_dict_row(domain: str, key: str, text: str) -> dict:
        composite_key = f"{domain}|{key}"
        return {
            "__metadata": _entity_metadata("DictionaryItem", "DictionaryItemSet", composite_key),
            "Domain": domain,
            "Key": key,
            "Text": text,
        }

    # Push $filter down to SQL for the DB-backed rows (this table can hold hundreds/thousands
    # of reference codes) - only the small, config-derived synthetic rows below (bounded by
    # config size, never grows with real data) are still filtered in Python, since they don't
    # exist as DB rows at all and can't be pushed down.
    query = db.query(DictionaryItem)
    if filter:
        try:
            expr = FilterParser.parse(DictionaryItem, filter, field_map={"Domain": "domain", "Key": "key", "Text": "text"})
        except FilterSyntaxError as exc:
            return _err(400, "VALIDATION_ERROR", f"Unsupported $filter expression: {exc}")
        if expr is not None:
            query = query.filter(expr)
    rows = [_to_dict_row(r.domain, r.key, r.text) for r in query.order_by(asc(DictionaryItem.domain), asc(DictionaryItem.key)).all()]

    config_rows = [_to_dict_row(r.get("Domain", ""), r.get("Key", ""), r.get("Text", "")) for r in _dictionary_config_rows(db)]
    if filter:
        try:
            pred = parse_filter_to_predicate(filter, {"Domain": "Domain", "Key": "Key", "Text": "Text"})
        except FilterSyntaxError as exc:
            return _err(400, "VALIDATION_ERROR", f"Unsupported $filter expression: {exc}")
        config_rows = [row for row in config_rows if pred(row)]
    rows.extend(config_rows)
    rows = _apply_orderby_rows(rows, orderby or "Domain asc,Key asc")
    total = len(rows)
    bounded_top, bounded_skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    paged = rows[bounded_skip: bounded_skip + bounded_top]
    return odata_payload(paged, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/PersonVHSet")
def person_vh_set(
    filter: str | None = Query(None, alias="$filter"),
    search: str | None = Query(None, alias="Search"),
    date_check: str | None = Query(None, alias="DateCheck"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    query = db.query(Person)
    expr = FilterParser.parse(Person, filter, field_map=PERSON_VH_MAP)
    if expr is not None:
        query = query.filter(expr)
    if date_check:
        active_date = _parse_odata_datetime(date_check).date()
        query = query.filter(
            Person.begda <= active_date,
            (Person.endda.is_(None)) | (Person.endda >= active_date),
        )
    query = query.order_by(asc(Person.last_name), asc(Person.first_name), asc(Person.middle_name), asc(Person.perner))
    rows = query.all()
    if search:
        sQuery = str(search).strip().lower()
        rows = [
            row for row in rows
            if sQuery in _person_full_name(row).lower() or sQuery in str(row.perner or "").lower()
        ]
    total = len(rows)
    top, skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    return odata_payload([_to_person_vh(row) for row in rows[skip: skip + top]], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/RuntimeSettingsSet")
def runtime_settings_set(db: Session = Depends(get_db)):
    item = SettingsService.load_global(SettingsRepo(db))
    item.setdefault("__metadata", _entity_metadata("RuntimeSettings", "RuntimeSettingsSet", item.get("Key", "GLOBAL")))
    return odata_collection([item])


@router.get(f"{SERVICE_ROOT}/RuntimeSettingsSet({{entity_key}})")
def runtime_settings_entity(entity_key: str, db: Session = Depends(get_db)):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("\'\"")
    if cleaned and cleaned.upper() != "GLOBAL":
        return _err(404, "NOT_FOUND", "Runtime settings not found")
    rows = runtime_settings_set(db)
    return odata_entity((rows.get("d", {}).get("results") or [])[0])
