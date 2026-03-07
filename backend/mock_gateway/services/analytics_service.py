import logging
import json
from collections import Counter, defaultdict
from datetime import datetime, timezone

from sqlalchemy.orm import Session, selectinload

from models import ChecklistRoot
from utils.time import now_utc

logger = logging.getLogger("gateway.analytics")

MONTH_LABELS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


class AnalyticsService:
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
    def _checklist_stats(root: ChecklistRoot) -> dict:
        a_checks = list(root.checks or [])
        a_barriers = list(root.barriers or [])
        i_failed_checks = 0
        i_success_checks = 0
        i_failed_barriers = 0
        i_success_barriers = 0
        for check in a_checks:
            if str(check.status or "").upper() in {"FAILED", "FAIL"}:
                i_failed_checks += 1
            else:
                i_success_checks += 1
        for barrier in a_barriers:
            if bool(barrier.is_active):
                i_success_barriers += 1
            else:
                i_failed_barriers += 1
        return {
            "failedChecks": i_failed_checks,
            "failedBarriers": i_failed_barriers,
            "successfulChecks": i_success_checks,
            "successfulBarriers": i_success_barriers,
            "checksTotal": len(a_checks),
            "barriersTotal": len(a_barriers),
            "status": str(root.status or "").upper(),
            "profession": AnalyticsService._normalize_text(root.observed_position, "Unknown profession"),
            "lpc": AnalyticsService._normalize_text(root.lpc_text or root.lpc, "Unknown LPC"),
            "location": AnalyticsService._normalize_text(root.location_text or root.location_name or root.location_key, "Unknown location"),
        }

    @staticmethod
    def _chart_rows(counter: Counter, limit: int = 8) -> list[dict]:
        rows = []
        for index, (label, value) in enumerate(sorted(counter.items(), key=lambda item: (-item[1], item[0]))[:limit], start=1):
            rows.append({
                "label": str(label),
                "value": int(value),
                "order": index,
            })
        return rows

    @staticmethod
    def _monthly_rows(grouped: defaultdict, selected_year: int, previous_year: int) -> list[dict]:
        rows = []
        for month in range(1, 13):
            s_label = AnalyticsService._month_label(month)
            for metric in ["TOTAL", "FAILED_CHECKS", "FAILED_BARRIERS"]:
                rows.append({
                    "dimension": "MONTHLY",
                    "metric": metric + "_SELECTED",
                    "bucketKey": f"{selected_year}-{month:02d}",
                    "label": s_label,
                    "value": int(grouped[(selected_year, month)].get(metric, 0)),
                    "order": month,
                })
                rows.append({
                    "dimension": "MONTHLY",
                    "metric": metric + "_PREVIOUS",
                    "bucketKey": f"{previous_year}-{month:02d}",
                    "label": s_label,
                    "value": int(grouped[(previous_year, month)].get(metric, 0)),
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
    def _compute_dashboard(db: Session, year: int | None = None) -> dict:
        selected_year = AnalyticsService._normalize_year(year)
        previous_year = selected_year - 1
        roots = AnalyticsService._base_roots_query(db).all()
        roots_selected = []
        totals_by_month = defaultdict(lambda: {"TOTAL": 0, "FAILED_CHECKS": 0, "FAILED_BARRIERS": 0})
        failed_checks_by_profession = Counter()
        failed_barriers_by_profession = Counter()
        failed_checks_by_lpc = Counter()
        failed_barriers_by_lpc = Counter()
        failed_checks_by_location = Counter()
        failed_barriers_by_location = Counter()
        totals_by_status = Counter()

        summary = {
            "selectedYear": selected_year,
            "previousYear": previous_year,
            "total": 0,
            "monthly": 0,
            "failedChecks": 0,
            "failedBarriers": 0,
            "closedCount": 0,
            "registeredCount": 0,
            "avgChecksRate": 0,
            "avgBarriersRate": 0,
            "healthy": 0,
            "refreshedAt": now_utc().isoformat() + "Z",
            "source": "backend",
            "availableYears": []
        }
        totals_all_years = Counter()
        i_total_checks = 0
        i_success_checks = 0
        i_total_barriers = 0
        i_success_barriers = 0

        for root in roots:
            root_year, root_month = AnalyticsService._resolve_root_date(root)
            totals_all_years[root_year] += 1
            if root_year not in {selected_year, previous_year}:
                continue
            stats = AnalyticsService._checklist_stats(root)
            totals_by_month[(root_year, root_month)]["TOTAL"] += 1
            totals_by_month[(root_year, root_month)]["FAILED_CHECKS"] += stats["failedChecks"]
            totals_by_month[(root_year, root_month)]["FAILED_BARRIERS"] += stats["failedBarriers"]
            if root_year != selected_year:
                continue

            roots_selected.append(root)
            summary["total"] += 1
            summary["failedChecks"] += stats["failedChecks"]
            summary["failedBarriers"] += stats["failedBarriers"]
            totals_by_status[stats["status"] or "UNKNOWN"] += 1
            if stats["status"] in {"DONE", "CLOSED"}:
                summary["closedCount"] += 1
            if stats["status"] in {"SUBMITTED", "REGISTERED"}:
                summary["registeredCount"] += 1
            i_total_checks += stats["checksTotal"]
            i_success_checks += stats["successfulChecks"]
            i_total_barriers += stats["barriersTotal"]
            i_success_barriers += stats["successfulBarriers"]
            if stats["failedChecks"]:
                failed_checks_by_profession[stats["profession"]] += stats["failedChecks"]
                failed_checks_by_lpc[stats["lpc"]] += stats["failedChecks"]
                failed_checks_by_location[stats["location"]] += stats["failedChecks"]
            if stats["failedBarriers"]:
                failed_barriers_by_profession[stats["profession"]] += stats["failedBarriers"]
                failed_barriers_by_lpc[stats["lpc"]] += stats["failedBarriers"]
                failed_barriers_by_location[stats["location"]] += stats["failedBarriers"]

        summary["monthly"] = totals_by_month[(selected_year, now_utc().month)]["TOTAL"] if selected_year == AnalyticsService._current_year() else 0
        summary["avgChecksRate"] = round((i_success_checks / i_total_checks) * 100, 2) if i_total_checks else 0
        summary["avgBarriersRate"] = round((i_success_barriers / i_total_barriers) * 100, 2) if i_total_barriers else 0
        summary["healthy"] = max(summary["total"] - summary["failedChecks"] - summary["failedBarriers"], 0)
        summary["availableYears"] = [
            {"key": str(i_year), "text": str(i_year)}
            for i_year in sorted(totals_all_years.keys(), reverse=True)
        ] or [{"key": str(selected_year), "text": str(selected_year)}]
        summary["availableYearsJson"] = json.dumps(summary["availableYears"], ensure_ascii=False)

        charts = (
            AnalyticsService._monthly_rows(totals_by_month, selected_year, previous_year)
            + AnalyticsService._breakdown_rows("PROFESSION", "FAILED_CHECKS", failed_checks_by_profession)
            + AnalyticsService._breakdown_rows("PROFESSION", "FAILED_BARRIERS", failed_barriers_by_profession)
            + AnalyticsService._breakdown_rows("LPC", "FAILED_CHECKS", failed_checks_by_lpc)
            + AnalyticsService._breakdown_rows("LPC", "FAILED_BARRIERS", failed_barriers_by_lpc)
            + AnalyticsService._breakdown_rows("LOCATION", "FAILED_CHECKS", failed_checks_by_location)
            + AnalyticsService._breakdown_rows("LOCATION", "FAILED_BARRIERS", failed_barriers_by_location)
            + AnalyticsService._breakdown_rows("STATUS", "TOTAL", totals_by_status)
        )

        logger.info(
            "Analytics computed year=%s total=%s failed_checks=%s failed_barriers=%s",
            selected_year,
            summary["total"],
            summary["failedChecks"],
            summary["failedBarriers"],
        )
        return dict(summary, charts=charts)

    @staticmethod
    def get_process_summary(db: Session, year: int | None = None) -> dict:
        dashboard = AnalyticsService._compute_dashboard(db, year)
        return {key: value for key, value in dashboard.items() if key != "charts"}

    @staticmethod
    def get_workflow_analytics(db: Session, year: int | None = None) -> dict:
        return AnalyticsService._compute_dashboard(db, year)

    @staticmethod
    def get_breakdown_rows(db: Session, year: int | None = None) -> list[dict]:
        return list((AnalyticsService._compute_dashboard(db, year) or {}).get("charts") or [])
