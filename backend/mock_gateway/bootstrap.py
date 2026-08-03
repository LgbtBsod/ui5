import json
import logging
import random
import sys
import uuid
from datetime import datetime, timedelta, timezone
from sqlalchemy import inspect, text
from sqlalchemy.exc import IntegrityError

from config import APP_PROFILE, AUTO_MUTATE_SCHEMA_ON_STARTUP, AUTO_SEED_STARTUP_DATA, FRONTEND_TIMER_PROFILE, PROMPT_LOGIN_ON_STARTUP_ENABLED
from database import Base, SessionLocal, engine
from models import (
    AnalyticsBreakdown,
    AnalyticsRefreshState,
    AnalyticsSnapshot,
    AppUserProfile,
    ChecklistBarrier,
    ChecklistCheck,
    ChecklistRoot,
    ChecklistRootDraft,
    FrontendRuntimeSettings,
    RuntimeUserContext,
)
from services.db_seed import seed_reference_data
from services.settings_service import DEFAULT_REQUIRED_FIELDS, DEFAULT_UPLOAD_POLICY

logger = logging.getLogger("gateway")


def _prompt_login_gui() -> str | None:
    """Blocking local Tk dialog asking for a username. Returns None (never raises) if a
    display isn't available, tkinter isn't installed, or the user cancels - callers must
    treat that as "keep whatever current user is already configured"."""
    try:
        import tkinter as tk
        from tkinter import simpledialog
    except Exception:
        logger.info("No GUI toolkit available; skipping startup login prompt")
        return None
    try:
        root = tk.Tk()
        root.withdraw()
        root.attributes("-topmost", True)
        uname = simpledialog.askstring(
            "SAP Gateway Simulator",
            "Enter your username (used as the current user for this mock session):",
            parent=root,
        )
        root.destroy()
        return str(uname or "").strip() or None
    except Exception:
        logger.exception("Startup login prompt failed; keeping existing current user")
        return None


def prompt_and_store_login() -> None:
    """Ask for a username once via a GUI dialog and persist it as RuntimeUserContext.CURRENT,
    which services.current_user_service.resolve_uname() already reads as its final fallback.
    A no-op under pytest, when explicitly disabled, or if a current user is already
    configured from a previous run.

    The pytest check is done here (call time, via sys.modules) rather than as a frozen
    config.py constant: config.py is imported once at process start, commonly before
    pytest has set PYTEST_CURRENT_TEST (which is set per-test, not at collection time), so
    a constant computed then would still be True during actual test runs, hanging every
    test that boots the app on this blocking GUI dialog. "pytest" being importable is
    stable regardless of import ordering.
    """
    if not PROMPT_LOGIN_ON_STARTUP_ENABLED or "pytest" in sys.modules:
        return
    db = SessionLocal()
    try:
        row = db.get(RuntimeUserContext, "CURRENT")
        if row and str(row.uname or "").strip():
            return
        uname = _prompt_login_gui()
        if not uname:
            return
        if row:
            row.uname = uname
        else:
            db.add(RuntimeUserContext(key="CURRENT", uname=uname))
        db.commit()
        logger.info("Startup login: current user set to %r", uname)
    finally:
        db.close()


def bootstrap_schema() -> None:
    """Create all tables and ensure schema compatibility."""
    Base.metadata.create_all(bind=engine)
    ensure_schema_compatibility()
    prompt_and_store_login()


def ensure_schema_compatibility() -> None:
    """Validate schema version and apply compatibility checks."""
    if not AUTO_MUTATE_SCHEMA_ON_STARTUP:
        logger.info("AUTO_MUTATE_SCHEMA_ON_STARTUP=false; skipping schema compatibility checks")
        return
    ensure_required_tables()
    db = SessionLocal()
    try:
        ensure_runtime_settings_row(db)
        if AUTO_SEED_STARTUP_DATA:
            ensure_baseline_mock_data(db)
    finally:
        db.close()


def ensure_required_tables(bind=None) -> None:
    """Create missing tables for analytics, settings, and runtime context.

    Accepts an optional `bind` so callers (e.g. tests wiring up an isolated
    in-memory database) can target a specific engine without mutating this
    module's own `engine` global, which would leak across every other caller
    for the rest of the process.
    """
    target = bind if bind is not None else engine
    inspector = inspect(target)
    existing_tables = set(inspector.get_table_names())
    required_tables = {
        AnalyticsSnapshot.__tablename__: AnalyticsSnapshot.__table__,
        AnalyticsBreakdown.__tablename__: AnalyticsBreakdown.__table__,
        AnalyticsRefreshState.__tablename__: AnalyticsRefreshState.__table__,
        FrontendRuntimeSettings.__tablename__: FrontendRuntimeSettings.__table__,
        AppUserProfile.__tablename__: AppUserProfile.__table__,
        RuntimeUserContext.__tablename__: RuntimeUserContext.__table__,
        ChecklistRootDraft.__tablename__: ChecklistRootDraft.__table__,
    }
    missing_tables = [table for name, table in required_tables.items() if name not in existing_tables]
    if not missing_tables:
        return
    for table in missing_tables:
        table.create(bind=target, checkfirst=True)
        logger.info("Created missing table: %s", table.name)


def ensure_runtime_settings_row(db) -> FrontendRuntimeSettings:
    """Ensure frontend runtime settings row exists.

    Read-then-insert has a TOCTOU gap under concurrent startup/requests: two callers can
    both see no row and both try to insert. There's no unique constraint stopping a second
    row (FrontendRuntimeSettings.id is a random UUID, not a natural singleton key), so a
    naive version of this would silently create duplicates and different callers would
    non-deterministically read whichever "first()" row wins. Re-query on IntegrityError to
    fall back to whatever row actually landed, rather than crashing or duplicating silently.
    """
    settings_row = db.query(FrontendRuntimeSettings).first()
    if settings_row:
        return settings_row
    settings_row = FrontendRuntimeSettings(
        environment=APP_PROFILE or "default",
        required_fields_json=json.dumps(DEFAULT_REQUIRED_FIELDS),
        upload_policy_json=json.dumps(DEFAULT_UPLOAD_POLICY),
        **FRONTEND_TIMER_PROFILE
    )
    db.add(settings_row)
    try:
        db.commit()
    except IntegrityError:
        db.rollback()
        settings_row = db.query(FrontendRuntimeSettings).first()
        if settings_row is None:
            raise
        return settings_row
    db.refresh(settings_row)
    return settings_row


def ensure_baseline_mock_data(db) -> None:
    """Seed baseline mock data if missing."""
    has_dictionary = db.execute(text("SELECT 1 FROM dictionary_items LIMIT 1")).first() is not None
    has_roots = db.execute(text("SELECT 1 FROM checklist_root LIMIT 1")).first() is not None
    has_runtime_user = db.execute(text("SELECT 1 FROM runtime_user_context LIMIT 1")).first() is not None
    if has_dictionary and has_roots and has_runtime_user:
        return
    seed_reference_data(db)
    seed_checklist_roots_if_needed(db, minimum_rows=100)
    ensure_integration_samples(db)
    ensure_seeded_detail_payload(db)


def seed_checklist_roots_if_needed(db, minimum_rows: int = 100) -> int:
    """Seed checklist root records if count is below minimum."""
    current_count = db.query(ChecklistRoot).filter((ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))).count()
    if current_count >= minimum_rows:
        return 0

    lpc_pool = ["LPC-01", "LPC-02", "LPC-03", "LPC-04", "LPC-05"]
    status_pool = ["DRAFT", "REGISTERED", "CLOSED"]
    equipment_pool = ["Pump", "Compressor", "Conveyor", "Boiler", "Generator"]
    bukrs_pool = ["1000", "2000", "3000"]
    observer_pool = ["Ivan Ivanov", "Anna Petrova", "Sergey Smirnov", "Olga Sokolova", "Petr Kuznetsov"]

    to_create = minimum_rows - current_count
    now = datetime.now(timezone.utc)
    for index in range(to_create):
        sequence = current_count + index + 1
        changed_on = now - timedelta(minutes=index)
        db.add(
            ChecklistRoot(
                id=str(uuid.uuid4()),
                checklist_id=f"CHK-{sequence:05d}",
                lpc=random.choice(lpc_pool),
                status=random.choice(status_pool),
                integration_flag=(sequence % 4 == 0),
                date=changed_on.date().isoformat(),
                time_check=changed_on.strftime("%H:%M"),
                time_zone="Europe/Saratov",
                equipment=random.choice(equipment_pool),
                bukrs=random.choice(bukrs_pool),
                observer_fullname=random.choice(observer_pool),
                changed_on=changed_on,
                changed_by="SYSTEM",
                version_number=1,
                created_on=changed_on,
                created_by="SYSTEM",
                is_deleted=False,
            )
        )
    db.commit()
    logger.info("Seeded checklist roots: added=%s total=%s", to_create, minimum_rows)
    return to_create


def seed_detail_payload_for_root(root: ChecklistRoot) -> tuple[list[ChecklistCheck], list[ChecklistBarrier]]:
    """Generate checks and barriers for a root record."""
    s_equipment = str(root.equipment or "Equipment").strip() or "Equipment"
    checks = [
        ChecklistCheck(
            root_id=root.id,
            text=f"Verify isolation for {s_equipment}",
            comment="Auto-seeded",
            status="PASS",
            position=1,
            created_on=root.created_on,
            changed_on=root.changed_on,
        ),
        ChecklistCheck(
            root_id=root.id,
            text=f"Confirm workplace condition around {s_equipment}",
            comment="Auto-seeded",
            status="PASS" if bool(root.integration_flag) else "FAIL",
            position=2,
            created_on=root.created_on,
            changed_on=root.changed_on,
        ),
        ChecklistCheck(
            root_id=root.id,
            text=f"Validate team briefing for {root.lpc or 'LPC'}",
            comment="Auto-seeded",
            status="PASS",
            position=3,
            created_on=root.created_on,
            changed_on=root.changed_on,
        ),
    ]
    barriers = [
        ChecklistBarrier(
            root_id=root.id,
            description=f"Protective barrier installed near {s_equipment}",
            comment="Auto-seeded",
            is_active=True,
            position=1,
            created_on=root.created_on,
            changed_on=root.changed_on,
        ),
        ChecklistBarrier(
            root_id=root.id,
            description="Work permit and supervisor acknowledgement confirmed",
            comment="Auto-seeded",
            is_active=not bool(root.integration_flag),
            position=2,
            created_on=root.created_on,
            changed_on=root.changed_on,
        ),
    ]
    return checks, barriers


def ensure_seeded_detail_payload(db) -> int:
    """Backfill checks and barriers for roots that don't have them."""
    bind = db.get_bind() if hasattr(db, "get_bind") else engine
    inspector = inspect(bind)
    existing_tables = set(inspector.get_table_names())
    required_tables = {"checklist_root", "checklist_check", "checklist_barrier"}
    if not required_tables.issubset(existing_tables):
        return 0

    roots = db.query(ChecklistRoot).filter(
        (ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))
    ).all()
    added = 0
    for root in roots:
        has_checks = db.query(ChecklistCheck.id).filter(ChecklistCheck.root_id == root.id).first() is not None
        has_barriers = db.query(ChecklistBarrier.id).filter(ChecklistBarrier.root_id == root.id).first() is not None
        if has_checks and has_barriers:
            continue
        checks, barriers = seed_detail_payload_for_root(root)
        if not has_checks:
            db.add_all(checks)
            added += len(checks)
        if not has_barriers:
            db.add_all(barriers)
            added += len(barriers)
    if added:
        db.commit()
        logger.info("Backfilled detail payload: added=%s", added)
    return added


def ensure_integration_samples(db, minimum_ratio: float = 0.18) -> int:
    """Ensure minimum ratio of integration-flag checklists."""
    roots = db.query(ChecklistRoot).filter(
        (ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False))
    ).order_by(ChecklistRoot.created_on.asc()).all()

    total = len(roots)
    if total == 0:
        return 0
    current_integration = [root for root in roots if bool(root.integration_flag)]
    target_count = max(1, int(total * float(minimum_ratio or 0)))
    if len(current_integration) >= target_count:
        return 0

    updated = 0
    for root in roots:
        if bool(root.integration_flag):
            continue
        root.integration_flag = True
        root.bukrs = ""
        root.location_key = ""
        root.location_name = ""
        root.location_text = ""
        root.observer_fullname = ""
        root.observer_perner = ""
        root.observer_position = ""
        root.observer_orgunit = ""
        updated += 1
        if len(current_integration) + updated >= target_count:
            break

    if updated:
        db.commit()
        logger.info("Updated integration flags: affected=%s target_ratio=%s", updated, minimum_ratio)
    return updated
