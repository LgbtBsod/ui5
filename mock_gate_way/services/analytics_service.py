import logging
from datetime import datetime

from sqlalchemy import func
from sqlalchemy.orm import Session

from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot

logger = logging.getLogger("gateway.analytics")


class AnalyticsService:
    @staticmethod
    def _month_prefix() -> str:
        return datetime.utcnow().strftime("%Y-%m")

    @staticmethod
    def get_process_summary(db: Session):
        month = AnalyticsService._month_prefix()
        total = db.query(func.count(ChecklistRoot.id)).filter(ChecklistRoot.is_deleted.isnot(True)).scalar() or 0
        monthly = db.query(func.count(ChecklistRoot.id)).filter(
            ChecklistRoot.is_deleted.isnot(True),
            ChecklistRoot.date.like(f"{month}%")
        ).scalar() or 0

        failed_checks_month = db.query(func.count(ChecklistCheck.id)).join(ChecklistRoot, ChecklistCheck.root_id == ChecklistRoot.id).filter(
            ChecklistRoot.is_deleted.isnot(True),
            ChecklistRoot.date.like(f"{month}%"),
            func.upper(ChecklistCheck.status) == "FAILED"
        ).scalar() or 0

        failed_barriers_month = db.query(func.count(ChecklistBarrier.id)).join(ChecklistRoot, ChecklistBarrier.root_id == ChecklistRoot.id).filter(
            ChecklistRoot.is_deleted.isnot(True),
            ChecklistRoot.date.like(f"{month}%"),
            ChecklistBarrier.is_active.is_(True)
        ).scalar() or 0

        logger.info(
            "Workflow analytics total=%s monthly=%s failed_checks_month=%s failed_barriers_month=%s month=%s",
            total,
            monthly,
            failed_checks_month,
            failed_barriers_month,
            month,
        )

        return {
            "total": int(total),
            "monthly": int(monthly),
            "failedChecks": int(failed_checks_month),
            "failedBarriers": int(failed_barriers_month),
            "healthy": max(int(monthly) - int(failed_checks_month) - int(failed_barriers_month), 0),
            "month": month,
            "refreshedAt": datetime.utcnow().isoformat() + "Z",
            "source": "backend",
        }
