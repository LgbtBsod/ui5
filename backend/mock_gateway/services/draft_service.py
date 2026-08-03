"""Fiori-style draft lifecycle for ChecklistRootSet (additive, parallel to the existing
session-lock flow in services/lock_service.py). Mirrors redux/serve.py's
_draft_prepare/_draft_activate/_draft_discard semantics, adapted to this codebase's
SQLAlchemy/hex32-key conventions - see api/gateway_draft_api.py for the thin route layer
that calls into this module."""
import uuid

from sqlalchemy.orm import Session

from api.gateway_helpers import BoundaryResolver, HEX_ZERO_GUID
from models import ChecklistRoot, ChecklistRootDraft
from services.analytics_service import AnalyticsService
from services.current_user_service import CurrentUserService
from utils.time import now_utc

_MIRRORED_COLUMNS = (
    "checklist_id", "lpc", "status", "integration_flag",
    "date", "time_check", "time_zone", "equipment", "bukrs", "lpc_text",
    "observer_fullname", "observer_perner", "observer_position", "observer_orgunit",
    "observer_integration_name",
    "observed_fullname", "observed_perner", "observed_position", "observed_orgunit",
    "observed_integration_name",
    "location_key", "location_name", "location_text",
)


def _copy_mirrored_columns(source, target) -> None:
    for column in _MIRRORED_COLUMNS:
        setattr(target, column, getattr(source, column))


class DraftService:
    @staticmethod
    def prepare(db: Session, active_uuid_expr: str) -> ChecklistRootDraft | None:
        """CheckRootPreparationAction: create-draft (no/zero ActiveUUID) or idempotent
        edit-draft (existing ActiveUUID - returns the already-in-progress draft unchanged
        if one exists, never clobbering it). Returns None if ActiveUUID names a real id
        that doesn't exist / is deleted."""
        resolved = BoundaryResolver.resolve_key(active_uuid_expr) if str(active_uuid_expr or "").strip() else ""

        if not resolved or resolved == HEX_ZERO_GUID:
            new_id = str(uuid.uuid4())
            draft = ChecklistRootDraft(
                id=new_id,
                active_id=None,
                root_id=new_id,
                created_by=CurrentUserService.resolve_uname(db=db),
                changed_by=CurrentUserService.resolve_uname(db=db),
            )
            db.add(draft)
            db.commit()
            return draft

        active = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == resolved,
            ChecklistRoot.is_deleted.isnot(True),
        ).first()
        if not active:
            return None

        existing = db.query(ChecklistRootDraft).filter(ChecklistRootDraft.active_id == active.id).first()
        if existing:
            return existing

        draft = ChecklistRootDraft(
            id=str(uuid.uuid4()),
            active_id=active.id,
            root_id=active.id,
            created_by=CurrentUserService.resolve_uname(db=db),
            changed_by=CurrentUserService.resolve_uname(db=db),
        )
        _copy_mirrored_columns(active, draft)
        db.add(draft)
        db.commit()
        return draft

    @staticmethod
    def activate(db: Session, draft_uuid_expr: str) -> ChecklistRoot | None:
        """CheckRootActivationAction: merges the draft into its active twin (or promotes
        a create-draft into a brand-new active row under the same id) and removes the
        draft row. Returns the (now-active) row, or None if the draft wasn't found."""
        resolved = BoundaryResolver.resolve_key(draft_uuid_expr)
        draft = db.query(ChecklistRootDraft).filter(ChecklistRootDraft.id == resolved).first()
        if not draft:
            return None

        user_name = CurrentUserService.resolve_uname(db=db)
        if draft.active_id:
            root = db.query(ChecklistRoot).filter(ChecklistRoot.id == draft.active_id).first()
            if root is None:
                db.delete(draft)
                db.commit()
                return None
            _copy_mirrored_columns(draft, root)
            root.changed_by = user_name or root.changed_by or "ANON"
            root.changed_on = now_utc()
            root.version_number = int(root.version_number or 0) + 1
        else:
            root = ChecklistRoot(
                id=draft.root_id,
                created_by=user_name,
                changed_by=user_name,
                version_number=1,
            )
            _copy_mirrored_columns(draft, root)
            db.add(root)

        db.delete(draft)
        db.commit()
        AnalyticsService.mark_dirty()
        db.refresh(root)
        return root

    @staticmethod
    def discard(db: Session, draft_uuid_expr: str) -> tuple[str, ChecklistRoot | None]:
        """CheckRootDiscardAction: removes the draft row without touching any active twin.
        Returns ("not_found", None), ("create_only", None) - i.e. a 204, no active twin
        ever existed - or ("kept_active", <untouched active row>) - i.e. a 200."""
        resolved = BoundaryResolver.resolve_key(draft_uuid_expr)
        draft = db.query(ChecklistRootDraft).filter(ChecklistRootDraft.id == resolved).first()
        if not draft:
            return "not_found", None

        active_id = draft.active_id
        db.delete(draft)
        db.commit()

        if not active_id:
            return "create_only", None

        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == active_id).first()
        return "kept_active", root
