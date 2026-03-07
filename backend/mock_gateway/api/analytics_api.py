import logging

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from models import AnalyticsBreakdown, AnalyticsSnapshot
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
    return {
        "__metadata": _entity_meta("SimpleAnalytical", "SimpleAnalyticalSet", "GLOBAL"),
        "Key": "GLOBAL",
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


def _to_breakdown_row(row: AnalyticsBreakdown) -> dict:
    composite_key = f"{row.dimension}|{row.metric}|{row.bucket_key}"
    return {
        "__metadata": _entity_meta("WorkflowAnalyticsBreakdown", "WorkflowAnalyticsBreakdownSet", composite_key),
        "Dimension": str(row.dimension or ""),
        "Metric": str(row.metric or ""),
        "BucketKey": str(row.bucket_key or ""),
        "Label": str(row.bucket_text or row.bucket_key or ""),
        "Value": int(row.metric_value or 0),
        "Order": int(row.sort_order or 0),
    }


@router.get("/WorkflowAnalytics")
def workflow_analytics(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_workflow_analytics(db)
    logger.info("WorkflowAnalytics payload=%s", payload)
    return payload


@router.get("/analytics/process")
def process_analytics(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    logger.info("analytics/process payload=%s", payload)
    return payload


@router.get("/SimpleAnalytical")
def simple_analytical(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    logger.info("SimpleAnalytical payload=%s", payload)
    return payload


@router.get(f"{SERVICE_ROOT}/SimpleAnalyticalSet")
def simple_analytical_set(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db)
    return odata_payload([_to_summary_row(payload)])


@router.get(f"{SERVICE_ROOT}/WorkflowAnalyticsBreakdownSet")
def workflow_analytics_breakdown_set(db: Session = Depends(get_db)):
    AnalyticsService._ensure_cache(db)
    snapshot = db.query(AnalyticsSnapshot).filter(AnalyticsSnapshot.month_key == AnalyticsService._month_key()).first()
    month_key = snapshot.month_key if snapshot else AnalyticsService._month_key()
    rows = db.query(AnalyticsBreakdown).filter(AnalyticsBreakdown.month_key == month_key).order_by(
        AnalyticsBreakdown.dimension.asc(),
        AnalyticsBreakdown.metric.asc(),
        AnalyticsBreakdown.sort_order.asc(),
    ).all()
    return odata_payload([_to_breakdown_row(row) for row in rows])
