from __future__ import annotations

from datetime import date, datetime

from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot, LastChangeSet, Person
from utils.odata import format_datetime

CHECKLIST_ROOT_MAP = {
    "Uuid": "id",
    "ChecklistId": "checklist_id",
    "Lpc": "lpc",
    "Status": "status",
    "Date": "date",
    "Equipment": "equipment",
    "ChangedOn": "changed_on",
    "ChangedBy": "changed_by",
    "CreatedOn": "created_on",
    "CreatedBy": "created_by",
    "VersionNumber": "version_number",
    "ObserverFullname": "observer_fullname",
    "ObserverPerner": "observer_perner",
    "ObserverPosition": "observer_position",
    "ObserverOrgunit": "observer_orgunit",
    "ObservedFullname": "observed_fullname",
    "ObservedPerner": "observed_perner",
    "ObservedPosition": "observed_position",
    "ObservedOrgunit": "observed_orgunit",
    "LocationKey": "location_key",
    "LocationName": "location_name",
}
CHECKLIST_FLAT_MAP = {
    "Uuid": "id",
    "ChecklistId": "checklist_id",
    "Lpc": "lpc",
    "Status": "status",
    "Date": "date",
    "Equipment": "equipment",
    "ChangedOn": "changed_on",
    "ChangedBy": "changed_by",
    "VersionNumber": "version_number",
}
CHECK_MAP = {
    "Uuid": "id",
    "ParentUuid": "root_id",
    "Text": "text",
    "Status": "status",
    "Position": "position",
    "ChangedOn": "changed_on",
    "EditMode": None,
}
BARRIER_MAP = {
    "Uuid": "id",
    "ParentUuid": "root_id",
    "Description": "description",
    "IsActive": "is_active",
    "Position": "position",
    "ChangedOn": "changed_on",
    "EditMode": None,
}
LAST_CHANGE_MAP = {
    "Uuid": "id",
    "EntityName": "entity_name",
    "EntityId": "entity_id",
    "ServerChangedOn": "last_change_timestamp",
}
PERSON_VH_MAP = {
    "Pernr": "perner",
    "FirstName": "first_name",
    "LastName": "last_name",
    "MiddleName": "middle_name",
    "Position": "position",
    "OrgUnit": "org_unit",
    "IntegrationName": "integration_name",
    "Begda": "begda",
    "Endda": "endda",
    "ChangedOn": "changed_on",
}


def parse_select(select: str | None) -> set[str] | None:
    if not select:
        return None
    values = {v.strip() for v in select.split(",") if v.strip()}
    return values or None


def _format(v):
    if isinstance(v, datetime):
        return format_datetime(v)
    if isinstance(v, date):
        return v.isoformat()
    return v


def _to_odata(item, mapping: dict[str, str | None], selected: set[str] | None = None, key_field: str = "Uuid"):
    select_set = (selected or set(mapping.keys())) | {key_field}
    out = {}
    for o_name, attr in mapping.items():
        if o_name not in select_set:
            continue
        if attr is None:
            out[o_name] = ""
            continue
        out[o_name] = _format(getattr(item, attr))
    return out


def root_to_odata(item: ChecklistRoot, selected: set[str] | None = None):
    return _to_odata(item, CHECKLIST_ROOT_MAP, selected)


def flat_to_odata(item: ChecklistRoot, selected: set[str] | None = None):
    return _to_odata(item, CHECKLIST_FLAT_MAP, selected)


def check_to_odata(item: ChecklistCheck, selected: set[str] | None = None):
    return _to_odata(item, CHECK_MAP, selected)


def barrier_to_odata(item: ChecklistBarrier, selected: set[str] | None = None):
    return _to_odata(item, BARRIER_MAP, selected)


def last_change_to_odata(item: LastChangeSet, selected: set[str] | None = None):
    return _to_odata(item, LAST_CHANGE_MAP, selected)


def person_to_odata(item: Person, selected: set[str] | None = None):
    return _to_odata(item, PERSON_VH_MAP, selected, key_field="Pernr")
