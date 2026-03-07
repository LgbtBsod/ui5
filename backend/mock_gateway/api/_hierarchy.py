from __future__ import annotations

from sqlalchemy.orm import Session

from api._date import parse_iso_date
from services.hierarchy_service import HierarchyService
from utils.odata import odata_payload


def hierarchy_tree_for_date(db: Session, date_value: str):
    return HierarchyService.get_tree(db, parse_iso_date(date_value))


def hierarchy_value_response(db: Session, date_value: str) -> dict:
    return {"value": hierarchy_tree_for_date(db, date_value)}


def hierarchy_odata_results_response(db: Session, date_value: str) -> dict:
    return odata_payload(hierarchy_tree_for_date(db, date_value))
