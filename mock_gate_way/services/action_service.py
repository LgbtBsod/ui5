from sqlalchemy.orm import Session

from models import ChecklistRoot
from utils.time import now_utc


class ActionService:
    @staticmethod
    def set_status(db: Session, root_id: str, status: str, user_id: str):
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.isnot(True)).first()
        if not root:
            raise ValueError("NOT_FOUND")
        root.status = status
        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        return {"status": "OK", "checklist_status": status}


    @staticmethod
    def export_report(db: Session, entity: str, filters: dict | None = None, search_mode: str = "EXACT"):
        filters = filters or {}
        query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
        roots = query.all()

        s_filter_id = str(filters.get("filterId") or "").lower().strip()
        s_filter_lpc = (filters.get("filterLpc") or "").strip()
        s_checks = filters.get("filterFailedChecks") or "ALL"
        s_barriers = filters.get("filterFailedBarriers") or "ALL"

        def match_flag(mode, failed):
            if mode == "ALL":
                return True
            return (mode == "TRUE" and failed) or (mode == "FALSE" and not failed)

        def is_match(root: ChecklistRoot) -> bool:
            checks_failed = any((c.status or "").upper() != "DONE" for c in (root.checks or []))
            barriers_failed = any(not bool(b.is_active) for b in (root.barriers or []))

            m_id = (not s_filter_id) or (s_filter_id in (root.checklist_id or "").lower()) or (s_filter_id in (root.id or "").lower())
            m_lpc = (not s_filter_lpc) or (root.lpc == s_filter_lpc)
            m_checks = match_flag(s_checks, checks_failed)
            m_barriers = match_flag(s_barriers, barriers_failed)

            if (search_mode or "EXACT").upper() == "LOOSE":
                enabled = []
                if s_filter_id:
                    enabled.append(m_id)
                if s_filter_lpc:
                    enabled.append(m_lpc)
                if s_checks != "ALL":
                    enabled.append(m_checks)
                if s_barriers != "ALL":
                    enabled.append(m_barriers)
                return any(enabled) if enabled else True

            return m_id and m_lpc and m_checks and m_barriers

        filtered = [r for r in roots if is_match(r)]
        rows = []

        for root in filtered:
            base = {
                "checklist_uuid": root.id,
                "checklist_id": root.checklist_id,
                "lpc": root.lpc,
                "status": root.status,
                "checks_total": len(root.checks or []),
                "barriers_total": len(root.barriers or []),
                "changed_on": root.changed_on.isoformat() if root.changed_on else "",
            }

            if entity == "barrier":
                for b in (root.barriers or []):
                    rows.append({
                        **base,
                        "barrier_uuid": b.id,
                        "barrier_position": b.position,
                        "barrier_desc": b.description,
                        "barrier_active": bool(b.is_active),
                    })
            elif entity == "check":
                for c in (root.checks or []):
                    rows.append({
                        **base,
                        "check_uuid": c.id,
                        "check_position": c.position,
                        "check_text": c.text,
                        "check_status": c.status,
                    })
            else:
                rows.append(base)

        return {"rows": rows, "count": len(rows), "entity": entity}
