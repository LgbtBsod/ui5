"""_DetailKind: the single place that encodes what distinguishes a ChecklistCheck row
from a ChecklistBarrier row (field names, kwargs shape, update/serialize functions).

Three call sites mutate check/barrier rows (SaveChanges/AutoSave batch upsert in
api.gateway_mutations, and the ChecklistCheckSet/ChecklistBarrierSet REST routes in
api.gateway_detail_api) - they used to each carry an independent, hand-duplicated copy
of "Check has text/comment/status(PASS|FAIL)/position, Barrier has description/comment/
is_active/position". This module is that mapping's only home.
"""

from dataclasses import dataclass
from typing import Callable

from api.gateway_serializers import _to_barrier, _to_check
from models import ChecklistBarrier, ChecklistCheck

CHECK_MAP = {
    "Key": "id",
    "ParentKey": "root_id",
    "PARENT_KEY": "root_id",
    "ChecksNum": "position",
    "ChangedOn": "changed_on",
}
BARRIER_MAP = {
    "Key": "id",
    "ParentKey": "root_id",
    "PARENT_KEY": "root_id",
    "BarriersNum": "position",
    "ChangedOn": "changed_on",
}


@dataclass(frozen=True)
class _DetailKind:
    model: type
    entity_tag: str  # "CHECK" / "BARRIER"
    not_found_message: str
    field_map: dict  # $filter/$orderby field map for the ChecklistCheckSet/BarrierSet REST list route
    key_aliases: tuple[str, ...]  # batch-upsert row key field names, checked in order
    text_aliases: tuple[str, ...]  # batch-upsert row text field names, checked in order
    number_aliases: tuple[str, ...]  # batch-upsert row position field names, checked in order
    number_field: str  # canonical PascalCase wire field name: "ChecksNum" / "BarriersNum"
    to_kwargs: Callable[[dict], dict]
    apply_update: Callable[[object, dict], None]
    serialize: Callable[..., dict]


def _check_kwargs(normalized: dict) -> dict:
    return {
        "text": normalized.get("text", ""),
        "comment": normalized.get("comment", ""),
        "status": "PASS" if normalized.get("result", True) else "FAIL",
        "position": normalized.get("position", 0),
    }


def _apply_check_update(row: ChecklistCheck, normalized: dict) -> None:
    if "text" in normalized:
        row.text = normalized["text"]
    if "comment" in normalized:
        row.comment = normalized["comment"]
    if "result" in normalized:
        row.status = "PASS" if normalized["result"] else "FAIL"
    if "position" in normalized:
        row.position = normalized["position"]


def _barrier_kwargs(normalized: dict) -> dict:
    return {
        "description": normalized.get("text", ""),
        "comment": normalized.get("comment", ""),
        "is_active": bool(normalized.get("result", True)),
        "position": normalized.get("position", 0),
    }


def _apply_barrier_update(row: ChecklistBarrier, normalized: dict) -> None:
    if "text" in normalized:
        row.description = normalized["text"]
    if "comment" in normalized:
        row.comment = normalized["comment"]
    if "result" in normalized:
        row.is_active = bool(normalized["result"])
    if "position" in normalized:
        row.position = normalized["position"]


_CHECK_KIND = _DetailKind(
    model=ChecklistCheck,
    entity_tag="CHECK",
    not_found_message="Check row not found",
    field_map=CHECK_MAP,
    key_aliases=("check_uuid", "Key", "key"),
    text_aliases=("text", "Text"),
    number_aliases=("checks_num", "ChecksNum", "position", "Position"),
    number_field="ChecksNum",
    to_kwargs=_check_kwargs,
    apply_update=_apply_check_update,
    serialize=_to_check,
)
_BARRIER_KIND = _DetailKind(
    model=ChecklistBarrier,
    entity_tag="BARRIER",
    not_found_message="Barrier row not found",
    field_map=BARRIER_MAP,
    key_aliases=("barrier_uuid", "Key", "key"),
    text_aliases=("text", "Text", "description", "Description"),
    number_aliases=("barriers_num", "BarriersNum", "position", "Position"),
    number_field="BarriersNum",
    to_kwargs=_barrier_kwargs,
    apply_update=_apply_barrier_update,
    serialize=_to_barrier,
)
