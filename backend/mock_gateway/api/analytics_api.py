import logging
import re

from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from database import get_db
from services.analytics_service import AnalyticsService, TASK_KEY_ANALYTICS_REFRESH
from utils.odata import ODATA_NS, SERVICE_ROOT, odata_error_response, odata_payload
from utils.odata_response import odata_entity

router = APIRouter(tags=["Analytics"])
logger = logging.getLogger("gateway.analytics.api")


def _entity_meta(entity_type: str, entity_set: str, key_value: str) -> dict:
    safe_key = str(key_value or "").replace("'", "''")
    return {
        "type": f"{ODATA_NS}.{entity_type}",
        "uri": f"{SERVICE_ROOT}/{entity_set}('{safe_key}')",
    }


def _to_function_result(payload: dict) -> dict:
    state = payload or {}
    task_key = str(state.get("taskKey") or TASK_KEY_ANALYTICS_REFRESH)
    status = str(state.get("status") or "IDLE").upper()
    message = str(state.get("lastMessage") or "")
    ok = status in {"REQUESTED", "RUNNING", "READY", "IDLE"}
    return {
        "__metadata": _entity_meta("FunctionResult", "AnalyticsRefreshTrigger", task_key),
        "RequestId": task_key,
        "Ok": ok,
        "Success": ok,
        "Message": message,
        "Action": "ANALYTICS_REFRESH",
        "Status": status,
        "Owner": str(state.get("requestedBy") or ""),
        "OwnerSession": str(state.get("activeRunId") or ""),
        "AggChangedOn": None,
        "ExpiresOn": None,
        "LockExpires": None,
        "ReasonCode": "ALREADY_RUNNING" if bool(state.get("isRunning")) else status,
    }


def _to_summary_row(payload: dict) -> dict:
    i_year = int(payload.get("selectedYear") or 0)
    source = str(payload.get("source") or "ALL")
    return {
        "__metadata": _entity_meta("SimpleAnalytical", "SimpleAnalyticalSet", f"{source}-{i_year}"),
        "Key": f"{source}-{i_year}",
        "SelectedYear": i_year,
        "PreviousYear": int(payload.get("previousYear") or max(i_year - 1, 0)),
        "AvailableYearsJson": str(payload.get("availableYearsJson") or "[]"),
        "Total": int(payload.get("total") or 0),
        "Monthly": int(payload.get("monthly") or 0),
        "FailedChecks": int(payload.get("failedChecks") or 0),
        "FailedBarriers": int(payload.get("failedBarriers") or 0),
        "FailedChecklistCount": int(payload.get("failedChecklistCount") or 0),
        "FailedBarrierChecklistCount": int(payload.get("failedBarrierChecklistCount") or 0),
        "ClosedCount": int(payload.get("closedCount") or 0),
        "RegisteredCount": int(payload.get("registeredCount") or 0),
        "AvgChecksRate": float(payload.get("avgChecksRate") or 0),
        "AvgBarriersRate": float(payload.get("avgBarriersRate") or 0),
        "Healthy": int(payload.get("healthy") or 0),
        "RefreshedAt": str(payload.get("refreshedAt") or "-"),
        "Source": source,
        "SourceText": str(payload.get("sourceText") or source),
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


def _to_refresh_state_row(payload: dict) -> dict:
    task_key = str(payload.get("taskKey") or TASK_KEY_ANALYTICS_REFRESH)
    return {
        "__metadata": _entity_meta("AnalyticsRefreshState", "AnalyticsRefreshStateSet", task_key),
        "TaskKey": task_key,
        "TaskName": str(payload.get("taskName") or task_key),
        "Status": str(payload.get("status") or "IDLE"),
        "IsRunning": bool(payload.get("isRunning")),
        "RequestedAt": str(payload.get("requestedAt") or ""),
        "RequestedBy": str(payload.get("requestedBy") or ""),
        "StartedAt": str(payload.get("startedAt") or ""),
        "FinishedAt": str(payload.get("finishedAt") or ""),
        "LastSuccessAt": str(payload.get("lastSuccessAt") or ""),
        "LastError": str(payload.get("lastError") or ""),
        "LastMessage": str(payload.get("lastMessage") or ""),
        "ActiveRunId": str(payload.get("activeRunId") or ""),
    }


def _parse_breakdown_filter(filter_expr: str | None) -> tuple[int | None, str | None]:
    s_filter = str(filter_expr or "").strip()
    if not s_filter:
        return None, None
    oYearMatch = re.search(r"SelectedYear\s+eq\s+(\d{4})", s_filter, flags=re.IGNORECASE)
    oSourceMatch = re.search(r"Source\s+eq\s+'([^']*)'", s_filter, flags=re.IGNORECASE)
    iYear = int(oYearMatch.group(1)) if oYearMatch else None
    sSource = oSourceMatch.group(1).replace("''", "'") if oSourceMatch else None
    return iYear, sSource


@router.get("/WorkflowAnalytics")
def workflow_analytics(year: int | None = None, source: str | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_workflow_analytics(db, year, source)
    logger.info("WorkflowAnalytics payload year=%s source=%s", year, source)
    return payload


@router.get("/analytics/process")
def process_analytics(year: int | None = None, source: str | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db, year, source)
    logger.info("analytics/process payload year=%s source=%s", year, source)
    return payload


@router.get("/SimpleAnalytical")
def simple_analytical(year: int | None = None, source: str | None = None, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_process_summary(db, year, source)
    logger.info("SimpleAnalytical payload year=%s source=%s", year, source)
    return payload


@router.get(f"{SERVICE_ROOT}/SimpleAnalyticalSet")
def simple_analytical_set(year: int | None = None, source: str | None = None, filter_expr: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    iFilterYear, sFilterSource = _parse_breakdown_filter(filter_expr)
    year = iFilterYear if iFilterYear is not None else year
    source = sFilterSource if sFilterSource is not None else source
    payload = AnalyticsService.get_process_summary(db, year, source)
    return odata_payload([_to_summary_row(payload)])


@router.get(f"{SERVICE_ROOT}/WorkflowAnalyticsBreakdownSet")
def workflow_analytics_breakdown_set(filter_expr: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    iYear, sSource = _parse_breakdown_filter(filter_expr)
    if not str(filter_expr or "").strip():
        return odata_error_response(400, "VALIDATION_ERROR", "WorkflowAnalyticsBreakdownSet requires $filter")
    rows = AnalyticsService.get_breakdown_rows(db, iYear, sSource)
    return odata_payload([_to_breakdown_row(row) for row in rows])


@router.get(f"{SERVICE_ROOT}/AnalyticsRefreshStateSet")
def analytics_refresh_state_set(db: Session = Depends(get_db)):
    payload = AnalyticsService.get_refresh_state(db)
    return odata_payload([_to_refresh_state_row(payload)])


@router.get(f"{SERVICE_ROOT}/AnalyticsRefreshStateSet('{{task_key}}')")
def analytics_refresh_state_entity(task_key: str, db: Session = Depends(get_db)):
    payload = AnalyticsService.get_refresh_state(db, task_key)
    return odata_entity(_to_refresh_state_row(payload))


@router.post(f"{SERVICE_ROOT}/AnalyticsRefreshTrigger")
def analytics_refresh_trigger(
    task_key: str | None = Query(None, alias="TaskKey"),
    requested_by: str | None = Query(None, alias="RequestedBy"),
    db: Session = Depends(get_db)
):
    payload = AnalyticsService.request_refresh(db, requested_by=requested_by, task_key=task_key)
    logger.info("Analytics refresh requested task=%s status=%s", payload.get("taskKey"), payload.get("status"))
    return odata_entity(_to_function_result(payload))
