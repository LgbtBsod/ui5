import logging

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.analytics_service import AnalyticsService
from utils.odata import ODATA_NS, SERVICE_ROOT, odata_payload

router = APIRouter(tags=["Analytics"])
logger = logging.getLogger("gateway.analytics.api")


def _entity_meta(entity_type: str, entity_set: str, key_value: str) -> dict:
    safe_key = str(key_value or "").replace("'", "''")
    return {
        "type": f"{ODATA_NS}.{entity_type}",
        "uri": f"{SERVICE_ROOT}/{entity_set}('{safe_key}')",
    }


def _to_summary_row(payload: dict) -> dict:
    i_year = int(payload.get("selectedYear") or 0)
    return {
        "__metadata": _entity_meta("SimpleAnalytical", "SimpleAnalyticalSet", f"GLOBAL-{i_year}"),
        "Key": f"GLOBAL-{i_year}",
        "SelectedYear": i_year,
        "PreviousYear": int(payload.get("previousYear") or max(i_year - 1, 0)),
        "AvailableYearsJson": str(payload.get("availableYearsJson") or "[]"),
        "Total": int(payload.get("total") or 0),
        "Monthly": int(payload.get("monthly") or 0),
        "FailedChecks": int(payload.get("failedChecks") or 0),
        "FailedBarriers": int(payload.get("failedBarriers") or 0),
        "ClosedCount": int(payload.get("closedCount") or 0),
        "RegisteredCount": int(payload.get("registeredCount") or 0),
        "AvgChecksRate": float(payload.get("avgChecksRate") or 0),
        "AvgBarriersRate": float(payload.get("avgBarriersRate") or 0),
        "Healthy": int(payload.get("healthy") or 0),
        "RefreshedAt": str(payload.get("refreshedAt") or "-"),
        "Source": str(payload.get("source") or "backend"),
    }


def _to_breakdown_row(row: dict) -> dict:
    composite_key = f"{row.get('dimension')}|{row.get('metric')}|{row.get('bucketKey')}"
    return {
        "__metadata": _entity_meta("WorkflowAnalyticsBreakdown", "WorkflowAnalyticsBreakdownSet", composite_key),
        "Dimension": str(row.get("dimension") or ""),
        "Metric": str(row.get("metric") or ""),
        "BucketKey": str(row.get("bucketKey") or ""),
        "Label": str(row.get("label") or row.get("bucketKey") or ""),
        "Value": int(row.get("value") or 0),
        "Order": int(row.get("order") or 0),
    }


@router.get("/WorkflowAnalytics")
def workflow_analytics(year: int | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_workflow_analytics(db, year)
    logger.info("WorkflowAnalytics payload year=%s", year)
    return payload


@router.get("/analytics/process")
def process_analytics(year: int | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db, year)
    logger.info("analytics/process payload year=%s", year)
    return payload


@router.get("/SimpleAnalytical")
def simple_analytical(year: int | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db, year)
    logger.info("SimpleAnalytical payload year=%s", year)
    return payload


@router.get(f"{SERVICE_ROOT}/SimpleAnalyticalSet")
def simple_analytical_set(year: int | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db, year)
    return odata_payload([_to_summary_row(payload)])


@router.get(f"{SERVICE_ROOT}/WorkflowAnalyticsBreakdownSet")
def workflow_analytics_breakdown_set(year: int | None = None, db: Session = Depends(get_db)):
    rows = AnalyticsService.get_breakdown_rows(db, year)
    return odata_payload([_to_breakdown_row(row) for row in rows])
