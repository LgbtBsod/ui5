import logging
from collections import Counter
from datetime import datetime, timezone

from sqlalchemy.orm import Session, selectinload

from models import AnalyticsBreakdown, AnalyticsSnapshot, ChecklistRoot
from utils.time import now_utc

logger = logging.getLogger("gateway.analytics")


class AnalyticsService:
    _dirty = True
    _last_refresh_at = None

    @staticmethod
    def _month_key() -> str:
        return datetime.now(timezone.utc).strftime("%Y-%m")

    @staticmethod
    def mark_dirty() -> None:
        AnalyticsService._dirty = True

    @staticmethod
    def should_refresh(interval_seconds: int) -> bool:
        if AnalyticsService._dirty or AnalyticsService._last_refresh_at is None:
            return True
        elapsed = (now_utc() - AnalyticsService._last_refresh_at).total_seconds()
        return elapsed >= max(5, int(interval_seconds or 0))

    @staticmethod
    def _base_roots_query(db: Session):
        return db.query(ChecklistRoot).options(
            selectinload(ChecklistRoot.checks),
            selectinload(ChecklistRoot.barriers),
        ).filter(ChecklistRoot.is_deleted.isnot(True))

    @staticmethod
    def _normalize_dimension_text(v_value: str, fallback_text: str) -> str:
        s_value = str(v_value or "").strip()
        return s_value or fallback_text

    @staticmethod
    def _chart_rows(counter: Counter) -> list[dict]:
        rows = []
        for index, (label, value) in enumerate(sorted(counter.items(), key=lambda item: (-item[1], item[0]))[:8], start=1):
            rows.append({
                "label": str(label),
                "value": int(value),
                "order": index,
            })
        return rows

    @staticmethod
    def refresh_cache(db: Session) -> dict:
        month_key = AnalyticsService._month_key()
        roots = AnalyticsService._base_roots_query(db).all()
        total = len(roots)
        monthly = 0
        closed_count = 0
        registered_count = 0
        failed_checks = 0
        failed_barriers = 0
        total_checks = 0
        successful_checks = 0
        total_barriers = 0
        successful_barriers = 0
        failed_checks_by_profession = Counter()
        failed_barriers_by_profession = Counter()
        failed_checks_by_lpc = Counter()
        failed_barriers_by_lpc = Counter()

        for root in roots:
            root_month_key = str(root.date or "")[:7]
            profession_text = AnalyticsService._normalize_dimension_text(root.observed_position, "Unknown profession")
            lpc_text = AnalyticsService._normalize_dimension_text(root.lpc_text or root.lpc, "Unknown LPC")
            status = str(root.status or "").upper()
            checks = list(root.checks or [])
            barriers = list(root.barriers or [])
            root_failed_checks = 0
            root_failed_barriers = 0

            if root_month_key == month_key:
                monthly += 1
            if status in {"DONE", "CLOSED"}:
                closed_count += 1
            if status in {"SUBMITTED", "REGISTERED"}:
                registered_count += 1

            total_checks += len(checks)
            total_barriers += len(barriers)

            for check in checks:
                if str(check.status or "").upper() in {"FAILED", "FAIL"}:
                    failed_checks += 1
                    root_failed_checks += 1
                else:
                    successful_checks += 1

            for barrier in barriers:
                if bool(barrier.is_active):
                    successful_barriers += 1
                else:
                    failed_barriers += 1
                    root_failed_barriers += 1

            if root_failed_checks:
                failed_checks_by_profession[profession_text] += root_failed_checks
                failed_checks_by_lpc[lpc_text] += root_failed_checks
            if root_failed_barriers:
                failed_barriers_by_profession[profession_text] += root_failed_barriers
                failed_barriers_by_lpc[lpc_text] += root_failed_barriers

        avg_checks_rate = round((successful_checks / total_checks) * 100, 2) if total_checks else 0
        avg_barriers_rate = round((successful_barriers / total_barriers) * 100, 2) if total_barriers else 0
        healthy_count = max(total - failed_checks - failed_barriers, 0)
        refreshed_at = now_utc()

        snapshot = db.query(AnalyticsSnapshot).filter(AnalyticsSnapshot.month_key == month_key).first()
        if not snapshot:
            snapshot = AnalyticsSnapshot(month_key=month_key)
            db.add(snapshot)
            db.flush()

        snapshot.total_checklists = int(total)
        snapshot.month_checklists = int(monthly)
        snapshot.failed_checks = int(failed_checks)
        snapshot.failed_barriers = int(failed_barriers)
        snapshot.closed_count = int(closed_count)
        snapshot.registered_count = int(registered_count)
        snapshot.avg_checks_rate = float(avg_checks_rate)
        snapshot.avg_barriers_rate = float(avg_barriers_rate)
        snapshot.healthy_count = int(healthy_count)
        snapshot.source = "backend"
        snapshot.refreshed_at = refreshed_at

        db.query(AnalyticsBreakdown).filter(AnalyticsBreakdown.month_key == month_key).delete()

        def add_breakdowns(dimension: str, metric: str, rows: list[dict]) -> None:
            for row in rows:
                db.add(AnalyticsBreakdown(
                    month_key=month_key,
                    dimension=dimension,
                    metric=metric,
                    bucket_key=str(row["label"]).upper(),
                    bucket_text=str(row["label"]),
                    metric_value=int(row["value"]),
                    sort_order=int(row["order"]),
                    changed_on=refreshed_at,
                ))

        add_breakdowns("PROFESSION", "FAILED_CHECKS", AnalyticsService._chart_rows(failed_checks_by_profession))
        add_breakdowns("PROFESSION", "FAILED_BARRIERS", AnalyticsService._chart_rows(failed_barriers_by_profession))
        add_breakdowns("LPC", "FAILED_CHECKS", AnalyticsService._chart_rows(failed_checks_by_lpc))
        add_breakdowns("LPC", "FAILED_BARRIERS", AnalyticsService._chart_rows(failed_barriers_by_lpc))

        db.commit()
        AnalyticsService._dirty = False
        AnalyticsService._last_refresh_at = refreshed_at

        logger.info(
            "Analytics cache refreshed month=%s total=%s monthly=%s failed_checks=%s failed_barriers=%s",
            month_key, total, monthly, failed_checks, failed_barriers,
        )
        return AnalyticsService.get_workflow_analytics(db)

    @staticmethod
    def _ensure_cache(db: Session) -> None:
        month_key = AnalyticsService._month_key()
        snapshot = db.query(AnalyticsSnapshot).filter(AnalyticsSnapshot.month_key == month_key).first()
        if snapshot is None or AnalyticsService._dirty:
            AnalyticsService.refresh_cache(db)

    @staticmethod
    def _summary_from_snapshot(snapshot: AnalyticsSnapshot | None) -> dict:
        if snapshot is None:
            return {
                "total": 0,
                "monthly": 0,
                "failedChecks": 0,
                "failedBarriers": 0,
                "closedCount": 0,
                "registeredCount": 0,
                "avgChecksRate": 0,
                "avgBarriersRate": 0,
                "healthy": 0,
                "refreshedAt": "-",
                "source": "backend",
            }
        return {
            "total": int(snapshot.total_checklists or 0),
            "monthly": int(snapshot.month_checklists or 0),
            "failedChecks": int(snapshot.failed_checks or 0),
            "failedBarriers": int(snapshot.failed_barriers or 0),
            "closedCount": int(snapshot.closed_count or 0),
            "registeredCount": int(snapshot.registered_count or 0),
            "avgChecksRate": float(snapshot.avg_checks_rate or 0),
            "avgBarriersRate": float(snapshot.avg_barriers_rate or 0),
            "healthy": int(snapshot.healthy_count or 0),
            "refreshedAt": (snapshot.refreshed_at or snapshot.created_on or now_utc()).isoformat() + "Z",
            "source": str(snapshot.source or "backend_aggregate"),
        }

    @staticmethod
    def get_process_summary(db: Session) -> dict:
        AnalyticsService._ensure_cache(db)
        snapshot = db.query(AnalyticsSnapshot).filter(AnalyticsSnapshot.month_key == AnalyticsService._month_key()).first()
        return AnalyticsService._summary_from_snapshot(snapshot)

    @staticmethod
    def get_workflow_analytics(db: Session) -> dict:
        AnalyticsService._ensure_cache(db)
        month_key = AnalyticsService._month_key()
        snapshot = db.query(AnalyticsSnapshot).filter(AnalyticsSnapshot.month_key == month_key).first()
        summary = AnalyticsService._summary_from_snapshot(snapshot)
        rows = db.query(AnalyticsBreakdown).filter(AnalyticsBreakdown.month_key == month_key).order_by(
            AnalyticsBreakdown.dimension.asc(),
            AnalyticsBreakdown.metric.asc(),
            AnalyticsBreakdown.sort_order.asc(),
        ).all()
        grouped = {
            "failedChecksByProfession": [],
            "failedBarriersByProfession": [],
            "failedChecksByLpc": [],
            "failedBarriersByLpc": [],
        }
        map_key = {
            ("PROFESSION", "FAILED_CHECKS"): "failedChecksByProfession",
            ("PROFESSION", "FAILED_BARRIERS"): "failedBarriersByProfession",
            ("LPC", "FAILED_CHECKS"): "failedChecksByLpc",
            ("LPC", "FAILED_BARRIERS"): "failedBarriersByLpc",
        }
        for row in rows:
            target = map_key.get((str(row.dimension or "").upper(), str(row.metric or "").upper()))
            if not target:
                continue
            grouped[target].append({
                "label": str(row.bucket_text or row.bucket_key or ""),
                "value": int(row.metric_value or 0),
                "order": int(row.sort_order or 0),
            })
        return dict(summary, charts=grouped)
