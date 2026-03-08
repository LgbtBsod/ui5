import json
import logging
import uuid
from collections import Counter, defaultdict
from datetime import datetime, timezone

from sqlalchemy.orm import Session, selectinload

from models import AnalyticsBreakdown, AnalyticsRefreshState, AnalyticsSnapshot, ChecklistRoot
from utils.time import now_utc

logger = logging.getLogger("gateway.analytics")

MONTH_LABELS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
TASK_KEY_ANALYTICS_REFRESH = "ANALYTICS_REFRESH"
TASK_NAME_ANALYTICS_REFRESH = "Analytics Refresh"
SOURCE_ALL = "ALL"
SOURCE_WEB = "WEB"
SOURCE_INTEGRATION = "INTEGRATION"
KNOWN_SOURCES = (SOURCE_ALL, SOURCE_WEB, SOURCE_INTEGRATION)


def _iso_z(value) -> str:
    if not value:
        return ""
    o_dt = value if getattr(value, "tzinfo", None) else value.replace(tzinfo=timezone.utc)
    return o_dt.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


class AnalyticsService:
    _dirty = False
    _last_success_at = None

    @staticmethod
    def _current_year() -> int:
        return datetime.now(timezone.utc).year

    @staticmethod
    def _normalize_year(i_year: int | None) -> int:
        try:
            i_value = int(i_year)
        except (TypeError, ValueError):
            i_value = AnalyticsService._current_year()
        return max(2020, min(2100, i_value))

    @staticmethod
    def _normalize_source(source: str | None) -> str:
        s_source = str(source or SOURCE_ALL).strip().upper()
        return s_source if s_source in KNOWN_SOURCES else SOURCE_ALL

    @staticmethod
    def _source_text(source: str | None) -> str:
        s_source = AnalyticsService._normalize_source(source)
        if s_source == SOURCE_WEB:
            return "Web"
        if s_source == SOURCE_INTEGRATION:
            return "Integration"
        return "All"

    @staticmethod
    def _scope_sources() -> list[str]:
        return [SOURCE_ALL, SOURCE_WEB, SOURCE_INTEGRATION]

    @staticmethod
    def _base_roots_query(db: Session):
        return db.query(ChecklistRoot).options(
            selectinload(ChecklistRoot.checks),
            selectinload(ChecklistRoot.barriers),
        ).filter(ChecklistRoot.is_deleted.isnot(True))

    @staticmethod
    def _normalize_text(v_value: str, fallback_text: str) -> str:
        s_value = str(v_value or "").strip()
        return s_value or fallback_text

    @staticmethod
    def _month_label(i_month: int) -> str:
        return MONTH_LABELS[max(1, min(12, int(i_month))) - 1]

    @staticmethod
    def _resolve_root_date(root: ChecklistRoot) -> tuple[int, int]:
        s_date = str(root.date or "").strip()
        if s_date:
            try:
                o_date = datetime.fromisoformat(s_date[:10])
                return o_date.year, o_date.month
            except ValueError:
                pass
        o_created = root.created_on or now_utc()
        return int(o_created.year), int(o_created.month)

    @staticmethod
    def _root_source(root: ChecklistRoot) -> str:
        return SOURCE_INTEGRATION if bool(root.integration_flag) else SOURCE_WEB

    @staticmethod
    def _matches_source(root: ChecklistRoot, source: str) -> bool:
        s_source = AnalyticsService._normalize_source(source)
        if s_source == SOURCE_ALL:
            return True
        return AnalyticsService._root_source(root) == s_source

    @staticmethod
    def _years_for_source(roots: list[ChecklistRoot], source: str) -> list[int]:
        years = sorted({
            AnalyticsService._resolve_root_date(root)[0]
            for root in (roots or [])
            if AnalyticsService._matches_source(root, source)
        }, reverse=True)
        return years or [AnalyticsService._current_year()]

    @staticmethod
    def _chart_rows(counter: Counter, limit: int = 10) -> list[dict]:
        rows = []
        for index, (label, value) in enumerate(sorted(counter.items(), key=lambda item: (-item[1], item[0]))[:limit], start=1):
            rows.append({
                "label": str(label),
                "value": int(value),
                "order": index,
            })
        return rows

    @staticmethod
    def _monthly_rows(grouped: defaultdict, selected_year: int) -> list[dict]:
        rows = []
        for month in range(1, 13):
            s_label = AnalyticsService._month_label(month)
            for metric in [
                "TOTAL",
                "FAILED_CHECKS",
                "FAILED_BARRIERS",
                "FAILED_CHECKLISTS",
                "FAILED_BARRIER_CHECKLISTS",
            ]:
                rows.append({
                    "dimension": "MONTHLY",
                    "metric": metric,
                    "bucketKey": f"{selected_year}-{month:02d}",
                    "label": s_label,
                    "value": int(grouped[(selected_year, month)].get(metric, 0)),
                    "order": month,
                })
        return rows

    @staticmethod
    def _breakdown_rows(dimension: str, metric: str, counter: Counter) -> list[dict]:
        return [
            {
                "dimension": dimension,
                "metric": metric,
                "bucketKey": str(row["label"]).upper(),
                "label": str(row["label"]),
                "value": int(row["value"]),
                "order": int(row["order"]),
            }
            for row in AnalyticsService._chart_rows(counter)
        ]

    @staticmethod
    def _compute_dashboard_from_roots(roots: list[ChecklistRoot], year: int | None = None, source: str | None = None) -> dict:
        selected_year = AnalyticsService._normalize_year(year)
        previous_year = selected_year - 1
        source_key = AnalyticsService._normalize_source(source)
        filtered_roots = [root for root in (roots or []) if AnalyticsService._matches_source(root, source_key)]

        totals_by_month = defaultdict(lambda: {
            "TOTAL": 0,
            "FAILED_CHECKS": 0,
            "FAILED_BARRIERS": 0,
            "FAILED_CHECKLISTS": 0,
            "FAILED_BARRIER_CHECKLISTS": 0,
        })
        failed_checks_by_profession = Counter()
        failed_barriers_by_profession = Counter()
        failed_checks_by_lpc = Counter()
        failed_barriers_by_lpc = Counter()
        failed_checks_by_location = Counter()
        failed_barriers_by_location = Counter()
        failed_checks_by_bukrs = Counter()
        failed_barriers_by_bukrs = Counter()
        failed_barriers_by_number = Counter()
        total_by_source = Counter()
        failed_checks_by_source = Counter()
        failed_barriers_by_source = Counter()
        totals_all_years = Counter()

        summary = {
            "selectedYear": selected_year,
            "previousYear": previous_year,
            "source": source_key,
            "sourceText": AnalyticsService._source_text(source_key),
            "total": 0,
            "monthly": 0,
            "failedChecks": 0,
            "failedBarriers": 0,
            "failedChecklistCount": 0,
            "failedBarrierChecklistCount": 0,
            "closedCount": 0,
            "registeredCount": 0,
            "avgChecksRate": 0,
            "avgBarriersRate": 0,
            "healthy": 0,
            "refreshedAt": _iso_z(now_utc()),
            "availableYears": [],
        }

        total_checks = 0
        success_checks = 0
        total_barriers = 0
        success_barriers = 0

        for root in filtered_roots:
            root_year, root_month = AnalyticsService._resolve_root_date(root)
            source_bucket = AnalyticsService._root_source(root)
            totals_all_years[root_year] += 1

            failed_checks = 0
            successful_checks = 0
            failed_barriers = 0
            successful_barriers = 0

            for check in list(root.checks or []):
                if str(check.status or "").upper() in {"FAILED", "FAIL"}:
                    failed_checks += 1
                else:
                    successful_checks += 1

            for barrier in list(root.barriers or []):
                if bool(barrier.is_active):
                    successful_barriers += 1
                    continue
                failed_barriers += 1
                barrier_key = str(int(barrier.position or 0)) if barrier.position else "UNKNOWN"
                barrier_label = ("#" + barrier_key) if barrier_key != "UNKNOWN" else "Unknown barrier"
                failed_barriers_by_number[barrier_label] += 1

            totals_by_month[(root_year, root_month)]["TOTAL"] += 1
            totals_by_month[(root_year, root_month)]["FAILED_CHECKS"] += failed_checks
            totals_by_month[(root_year, root_month)]["FAILED_BARRIERS"] += failed_barriers
            if failed_checks > 0:
                totals_by_month[(root_year, root_month)]["FAILED_CHECKLISTS"] += 1
            if failed_barriers > 0:
                totals_by_month[(root_year, root_month)]["FAILED_BARRIER_CHECKLISTS"] += 1
            total_by_source[source_bucket] += 1
            failed_checks_by_source[source_bucket] += failed_checks
            failed_barriers_by_source[source_bucket] += failed_barriers

            if root_year != selected_year:
                continue

            profession = AnalyticsService._normalize_text(root.observed_position, "Unknown profession")
            lpc = AnalyticsService._normalize_text(root.lpc_text or root.lpc, "Unknown LPC")
            location = AnalyticsService._normalize_text(root.location_text or root.location_name or root.location_key, "Unknown location")
            bukrs = AnalyticsService._normalize_text(root.bukrs, "Unknown BUKRS")
            status = str(root.status or "").upper()

            summary["total"] += 1
            summary["failedChecks"] += failed_checks
            summary["failedBarriers"] += failed_barriers
            if failed_checks > 0:
                summary["failedChecklistCount"] += 1
                failed_checks_by_profession[profession] += failed_checks
                failed_checks_by_lpc[lpc] += failed_checks
                failed_checks_by_location[location] += failed_checks
                failed_checks_by_bukrs[bukrs] += failed_checks
            if failed_barriers > 0:
                summary["failedBarrierChecklistCount"] += 1
                failed_barriers_by_profession[profession] += failed_barriers
                failed_barriers_by_lpc[lpc] += failed_barriers
                failed_barriers_by_location[location] += failed_barriers
                failed_barriers_by_bukrs[bukrs] += failed_barriers
            if status in {"DONE", "CLOSED"}:
                summary["closedCount"] += 1
            if status in {"SUBMITTED", "REGISTERED"}:
                summary["registeredCount"] += 1

            total_checks += len(root.checks or [])
            success_checks += successful_checks
            total_barriers += len(root.barriers or [])
            success_barriers += successful_barriers

        summary["monthly"] = totals_by_month[(selected_year, now_utc().month)]["TOTAL"] if selected_year == AnalyticsService._current_year() else 0
        summary["avgChecksRate"] = round((success_checks / total_checks) * 100, 2) if total_checks else 0
        summary["avgBarriersRate"] = round((success_barriers / total_barriers) * 100, 2) if total_barriers else 0
        summary["healthy"] = max(summary["total"] - summary["failedChecklistCount"] - summary["failedBarrierChecklistCount"], 0)
        summary["availableYears"] = [{"key": str(year_value), "text": str(year_value)} for year_value in sorted(totals_all_years.keys(), reverse=True)] or [{"key": str(selected_year), "text": str(selected_year)}]
        summary["availableYearsJson"] = json.dumps(summary["availableYears"], ensure_ascii=False)

        charts = (
            AnalyticsService._monthly_rows(totals_by_month, selected_year)
            + AnalyticsService._breakdown_rows("PROFESSION", "FAILED_CHECKS", failed_checks_by_profession)
            + AnalyticsService._breakdown_rows("PROFESSION", "FAILED_BARRIERS", failed_barriers_by_profession)
            + AnalyticsService._breakdown_rows("LPC", "FAILED_CHECKS", failed_checks_by_lpc)
            + AnalyticsService._breakdown_rows("LPC", "FAILED_BARRIERS", failed_barriers_by_lpc)
            + AnalyticsService._breakdown_rows("LOCATION", "FAILED_CHECKS", failed_checks_by_location)
            + AnalyticsService._breakdown_rows("LOCATION", "FAILED_BARRIERS", failed_barriers_by_location)
            + AnalyticsService._breakdown_rows("BUKRS", "FAILED_CHECKS", failed_checks_by_bukrs)
            + AnalyticsService._breakdown_rows("BUKRS", "FAILED_BARRIERS", failed_barriers_by_bukrs)
            + AnalyticsService._breakdown_rows("BARRIER_NUMBER", "FAILED_BARRIERS", failed_barriers_by_number)
            + AnalyticsService._breakdown_rows("SOURCE", "TOTAL", total_by_source)
            + AnalyticsService._breakdown_rows("SOURCE", "FAILED_CHECKS", failed_checks_by_source)
            + AnalyticsService._breakdown_rows("SOURCE", "FAILED_BARRIERS", failed_barriers_by_source)
        )

        return dict(summary, charts=charts)

    @staticmethod
    def _ensure_refresh_state(db: Session, task_key: str | None = None) -> AnalyticsRefreshState:
        resolved_key = str(task_key or TASK_KEY_ANALYTICS_REFRESH).strip().upper() or TASK_KEY_ANALYTICS_REFRESH
        row = db.query(AnalyticsRefreshState).filter(AnalyticsRefreshState.task_key == resolved_key).first()
        if row:
            return row
        row = AnalyticsRefreshState(
            task_key=resolved_key,
            task_name=TASK_NAME_ANALYTICS_REFRESH if resolved_key == TASK_KEY_ANALYTICS_REFRESH else resolved_key,
            status="IDLE",
            is_running=False,
            last_message="Waiting for refresh"
        )
        db.add(row)
        db.commit()
        db.refresh(row)
        return row

    @staticmethod
    def _serialize_refresh_state(row: AnalyticsRefreshState | None) -> dict:
        state = row or AnalyticsRefreshState(task_key=TASK_KEY_ANALYTICS_REFRESH, task_name=TASK_NAME_ANALYTICS_REFRESH)
        return {
            "taskKey": str(state.task_key or TASK_KEY_ANALYTICS_REFRESH),
            "taskName": str(state.task_name or TASK_NAME_ANALYTICS_REFRESH),
            "status": str(state.status or "IDLE").upper(),
            "isRunning": bool(state.is_running),
            "requestedAt": _iso_z(state.requested_at),
            "requestedBy": str(state.requested_by or ""),
            "startedAt": _iso_z(state.started_at),
            "finishedAt": _iso_z(state.finished_at),
            "lastSuccessAt": _iso_z(state.last_success_at),
            "lastError": str(state.last_error or ""),
            "lastMessage": str(state.last_message or ""),
            "activeRunId": str(state.active_run_id or ""),
        }

    @staticmethod
    def _scope_key(selected_year: int, source_key: str) -> str:
        return f"{AnalyticsService._normalize_source(source_key)}:{AnalyticsService._normalize_year(selected_year)}"

    @staticmethod
    def _persist_dashboard(db: Session, summary: dict, charts: list[dict]) -> None:
        snapshot = AnalyticsSnapshot(
            month_key=AnalyticsService._scope_key(summary.get("selectedYear"), summary.get("source")),
            selected_year=int(summary.get("selectedYear") or 0),
            source_key=AnalyticsService._normalize_source(summary.get("source")),
            available_years_json=str(summary.get("availableYearsJson") or "[]"),
            total_checklists=int(summary.get("total") or 0),
            month_checklists=int(summary.get("monthly") or 0),
            failed_checks=int(summary.get("failedChecks") or 0),
            failed_barriers=int(summary.get("failedBarriers") or 0),
            failed_checklist_count=int(summary.get("failedChecklistCount") or 0),
            failed_barrier_checklist_count=int(summary.get("failedBarrierChecklistCount") or 0),
            closed_count=int(summary.get("closedCount") or 0),
            registered_count=int(summary.get("registeredCount") or 0),
            avg_checks_rate=float(summary.get("avgChecksRate") or 0),
            avg_barriers_rate=float(summary.get("avgBarriersRate") or 0),
            healthy_count=int(summary.get("healthy") or 0),
            source=str(summary.get("source") or SOURCE_ALL),
            refreshed_at=now_utc(),
        )
        db.add(snapshot)
        for row in charts or []:
            db.add(AnalyticsBreakdown(
                month_key=snapshot.month_key,
                selected_year=snapshot.selected_year,
                source_key=snapshot.source_key,
                dimension=str(row.get("dimension") or ""),
                metric=str(row.get("metric") or ""),
                bucket_key=str(row.get("bucketKey") or ""),
                bucket_text=str(row.get("label") or row.get("bucketKey") or ""),
                metric_value=int(row.get("value") or 0),
                sort_order=int(row.get("order") or 0),
            ))

    @staticmethod
    def _ensure_cached(db: Session, year: int | None = None, source: str | None = None) -> None:
        selected_year = AnalyticsService._normalize_year(year)
        source_key = AnalyticsService._normalize_source(source)
        exists = db.query(AnalyticsSnapshot).filter(
            AnalyticsSnapshot.selected_year == selected_year,
            AnalyticsSnapshot.source_key == source_key,
        ).first()
        if exists:
            return
        AnalyticsService.refresh_cache(db, trigger="lazy-load")

    @staticmethod
    def _read_snapshot(db: Session, year: int | None = None, source: str | None = None) -> tuple[dict, list[dict]]:
        selected_year = AnalyticsService._normalize_year(year)
        source_key = AnalyticsService._normalize_source(source)
        AnalyticsService._ensure_cached(db, selected_year, source_key)
        snapshot = db.query(AnalyticsSnapshot).filter(
            AnalyticsSnapshot.selected_year == selected_year,
            AnalyticsSnapshot.source_key == source_key,
        ).order_by(AnalyticsSnapshot.refreshed_at.desc()).first()
        if not snapshot:
            return AnalyticsService._compute_dashboard_from_roots([], selected_year, source_key), []

        try:
            available_years = json.loads(str(snapshot.available_years_json or "[]"))
        except Exception:
            available_years = []

        summary = {
            "selectedYear": int(snapshot.selected_year or selected_year),
            "previousYear": int(snapshot.selected_year or selected_year) - 1,
            "availableYears": available_years if isinstance(available_years, list) else [],
            "availableYearsJson": str(snapshot.available_years_json or "[]"),
            "total": int(snapshot.total_checklists or 0),
            "monthly": int(snapshot.month_checklists or 0),
            "failedChecks": int(snapshot.failed_checks or 0),
            "failedBarriers": int(snapshot.failed_barriers or 0),
            "failedChecklistCount": int(snapshot.failed_checklist_count or 0),
            "failedBarrierChecklistCount": int(snapshot.failed_barrier_checklist_count or 0),
            "closedCount": int(snapshot.closed_count or 0),
            "registeredCount": int(snapshot.registered_count or 0),
            "avgChecksRate": float(snapshot.avg_checks_rate or 0),
            "avgBarriersRate": float(snapshot.avg_barriers_rate or 0),
            "healthy": int(snapshot.healthy_count or 0),
            "refreshedAt": _iso_z(snapshot.refreshed_at),
            "source": AnalyticsService._normalize_source(snapshot.source_key or snapshot.source),
            "sourceText": AnalyticsService._source_text(snapshot.source_key or snapshot.source),
        }
        rows = db.query(AnalyticsBreakdown).filter(
            AnalyticsBreakdown.selected_year == selected_year,
            AnalyticsBreakdown.source_key == source_key,
        ).order_by(
            AnalyticsBreakdown.dimension.asc(),
            AnalyticsBreakdown.metric.asc(),
            AnalyticsBreakdown.sort_order.asc(),
            AnalyticsBreakdown.bucket_text.asc(),
        ).all()
        charts = [{
            "dimension": str(row.dimension or ""),
            "metric": str(row.metric or ""),
            "bucketKey": str(row.bucket_key or ""),
            "label": str(row.bucket_text or row.bucket_key or ""),
            "value": int(row.metric_value or 0),
            "order": int(row.sort_order or 0),
        } for row in rows]
        return summary, charts

    @staticmethod
    def mark_dirty() -> None:
        AnalyticsService._dirty = True

    @staticmethod
    def should_refresh(db: Session, max_age_seconds: int) -> bool:
        row = AnalyticsService._ensure_refresh_state(db)
        if bool(row.is_running):
            return False
        if str(row.status or "").upper() == "REQUESTED":
            return True
        if AnalyticsService._dirty:
            return True
        if not AnalyticsService._last_success_at:
            return True
        age = (now_utc() - AnalyticsService._last_success_at).total_seconds()
        return age >= max(5, int(max_age_seconds or 0))

    @staticmethod
    def get_refresh_state(db: Session, task_key: str | None = None) -> dict:
        return AnalyticsService._serialize_refresh_state(AnalyticsService._ensure_refresh_state(db, task_key))

    @staticmethod
    def request_refresh(db: Session, requested_by: str | None = None, task_key: str | None = None) -> dict:
        row = AnalyticsService._ensure_refresh_state(db, task_key)
        now = now_utc()
        if str(row.status or "").upper() == "REQUESTED":
            row.last_message = "Refresh is already queued"
            db.commit()
            db.refresh(row)
            return AnalyticsService._serialize_refresh_state(row)
        if bool(row.is_running):
            row.last_message = "Refresh is already running"
            db.commit()
            db.refresh(row)
            return AnalyticsService._serialize_refresh_state(row)

        row.status = "REQUESTED"
        row.requested_at = now
        row.requested_by = str(requested_by or row.requested_by or "ANON")
        row.last_error = ""
        row.last_message = "Refresh requested"
        row.changed_on = now
        db.commit()
        db.refresh(row)
        AnalyticsService._dirty = True
        return AnalyticsService._serialize_refresh_state(row)

    @staticmethod
    def refresh_cache(db: Session, trigger: str | None = None, requested_by: str | None = None) -> dict:
        row = AnalyticsService._ensure_refresh_state(db)
        if bool(row.is_running):
            return AnalyticsService._serialize_refresh_state(row)

        now = now_utc()
        row.status = "RUNNING"
        row.is_running = True
        row.started_at = now
        row.finished_at = None
        row.last_error = ""
        row.last_message = f"Refresh started by {trigger or 'scheduler'}"
        row.active_run_id = str(uuid.uuid4())
        if requested_by:
            row.requested_by = str(requested_by or "")
        db.commit()

        try:
            roots = AnalyticsService._base_roots_query(db).all()
            db.query(AnalyticsBreakdown).delete()
            db.query(AnalyticsSnapshot).delete()

            for source_key in AnalyticsService._scope_sources():
                years = AnalyticsService._years_for_source(roots, source_key)
                for selected_year in years:
                    dashboard = AnalyticsService._compute_dashboard_from_roots(roots, selected_year, source_key)
                    AnalyticsService._persist_dashboard(db, {key: value for key, value in dashboard.items() if key != "charts"}, dashboard.get("charts") or [])

            row.status = "READY"
            row.is_running = False
            row.finished_at = now_utc()
            row.last_success_at = row.finished_at
            row.last_error = ""
            row.last_message = "Refresh completed"
            row.active_run_id = ""
            db.commit()
            db.refresh(row)
            AnalyticsService._dirty = False
            AnalyticsService._last_success_at = row.last_success_at
            logger.info("Analytics cache refreshed trigger=%s", trigger or "scheduler")
            return AnalyticsService._serialize_refresh_state(row)
        except Exception as error:
            db.rollback()
            row = AnalyticsService._ensure_refresh_state(db)
            row.status = "ERROR"
            row.is_running = False
            row.finished_at = now_utc()
            row.last_error = str(error)
            row.last_message = "Refresh failed"
            row.active_run_id = ""
            db.commit()
            db.refresh(row)
            AnalyticsService._dirty = True
            logger.exception("Failed to refresh analytics cache")
            raise

    @staticmethod
    def get_process_summary(db: Session, year: int | None = None, source: str | None = None) -> dict:
        summary, _charts = AnalyticsService._read_snapshot(db, year, source)
        return summary

    @staticmethod
    def get_workflow_analytics(db: Session, year: int | None = None, source: str | None = None) -> dict:
        summary, charts = AnalyticsService._read_snapshot(db, year, source)
        return dict(summary, charts=charts, refreshState=AnalyticsService.get_refresh_state(db))

    @staticmethod
    def get_breakdown_rows(db: Session, year: int | None = None, source: str | None = None) -> list[dict]:
        _summary, charts = AnalyticsService._read_snapshot(db, year, source)
        return charts
