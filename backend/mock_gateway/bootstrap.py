import json
import logging
import random
import uuid
from datetime import datetime, timedelta, timezone
from sqlalchemy import inspect, text

from config import APP_PROFILE, AUTO_MUTATE_SCHEMA_ON_STARTUP, AUTO_SEED_STARTUP_DATA, FRONTEND_TIMER_PROFILE
from database import Base, SessionLocal, engine
from models import (
    AnalyticsBreakdown,
    AnalyticsRefreshState,
    AnalyticsSnapshot,
    AppUserProfile,
    ChecklistBarrier,
    ChecklistCheck,
    ChecklistRoot,
    FrontendRuntimeSettings,
    RuntimeUserContext,
)
from services.db_seed import seed_reference_data
from services.settings_service import DEFAULT_REQUIRED_FIELDS, DEFAULT_UPLOAD_POLICY

logger = logging.getLogger("gateway")


def bootstrap_schema() -> None:
    """Create all tables and ensure schema compatibility."""
    Base.metadata.create_all(bind=engine)
    ensure_schema_compatibility()


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


def ensure_required_tables() -> None:
    """Create missing tables for analytics, settings, and runtime context."""
    inspector = inspect(engine)
    existing_tables = set(inspector.get_table_names())
    required_tables = {
        AnalyticsSnapshot.__tablename__: AnalyticsSnapshot.__table__,
        AnalyticsBreakdown.__tablename__: AnalyticsBreakdown.__table__,
        AnalyticsRefreshState.__tablename__: AnalyticsRefreshState.__table__,
        FrontendRuntimeSettings.__tablename__: FrontendRuntimeSettings.__table__,
        AppUserProfile.__tablename__: AppUserProfile.__table__,
        RuntimeUserContext.__tablename__: RuntimeUserContext.__table__,
    }
    missing_tables = [table for name, table in required_tables.items() if name not in existing_tables]
    if not missing_tables:
        return
    for table in missing_tables:
        table.create(bind=engine, checkfirst=True)
        logger.info("Created missing table: %s", table.name)


def ensure_runtime_settings_row(db) -> FrontendRuntimeSettings:
    """Ensure frontend runtime settings row exists."""
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
    db.commit()
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
