import uuid

from sqlalchemy import Boolean, Column, Date, DateTime, Float, ForeignKey, Index, String, Text, text
from sqlalchemy.dialects.sqlite import INTEGER
from sqlalchemy.orm import relationship

from config import FRONTEND_TIMER_PROFILE
from database import Base
from utils.time import now_utc


class ChecklistRoot(Base):
    __tablename__ = "checklist_root"
    __table_args__ = (
        # Every list/search query filters on is_deleted, almost always combined with
        # status and/or bukrs (ChecklistSearchSet, ChecklistRootSet, report_export).
        # Without these, each of those queries is a full table scan.
        Index("ix_checklist_root_is_deleted_status", "is_deleted", "status"),
        Index("ix_checklist_root_bukrs", "bukrs"),
    )

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    checklist_id = Column(String, nullable=False)
    lpc = Column(String, nullable=False)
    status = Column(String, default="01")
    integration_flag = Column(Boolean, default=False)

    date = Column(String, default="")
    time_check = Column(String, default="")
    time_zone = Column(String, default="")
    equipment = Column(String, default="")
    bukrs = Column(String, default="")
    lpc_text = Column(String, default="")

    observer_fullname = Column(String, default="")
    observer_perner = Column(String, default="")
    observer_position = Column(String, default="")
    observer_orgunit = Column(String, default="")
    observer_integration_name = Column(String, default="")

    observed_fullname = Column(String, default="")
    observed_perner = Column(String, default="")
    observed_position = Column(String, default="")
    observed_orgunit = Column(String, default="")
    observed_integration_name = Column(String, default="")

    location_key = Column(String, default="")
    location_name = Column(String, default="")
    location_text = Column(String, default="")

    created_on = Column(DateTime, default=now_utc)
    created_by = Column(String)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)
    changed_by = Column(String)
    version_number = Column(INTEGER, default=1)

    is_deleted = Column(Boolean, default=False)

    checks = relationship(
        "ChecklistCheck",
        back_populates="root",
        cascade="all, delete-orphan",
    )
    barriers = relationship(
        "ChecklistBarrier",
        back_populates="root",
        cascade="all, delete-orphan",
    )


class ChecklistRootDraft(Base):
    """Fiori-style draft sibling of ChecklistRoot (additive, parallel concurrency model to
    the LockEntry/LockService session-lock flow - see api/gateway_draft_api.py and
    services/draft_service.py). Mirrors ChecklistRoot's editable columns 1:1 so
    _apply_root_payload/_apply_basic_payload (api/gateway_operations.py,
    api/gateway_validators.py) work unmodified against either model.

    active_id is NULL for a create-draft (no active twin exists yet); root_id is the
    stable id this draft becomes on activation - this row's own id for a create-draft,
    active_id for an edit-draft (mirrors serve.py's RootId/DraftUUID reuse on create).
    """

    __tablename__ = "checklist_root_draft"
    __table_args__ = (Index("ix_checklist_root_draft_active_id", "active_id"),)

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    active_id = Column(String, ForeignKey("checklist_root.id"), nullable=True)
    root_id = Column(String, nullable=False)

    checklist_id = Column(String, nullable=False, default="")
    lpc = Column(String, nullable=False, default="")
    status = Column(String, default="01")
    integration_flag = Column(Boolean, default=False)

    date = Column(String, default="")
    time_check = Column(String, default="")
    time_zone = Column(String, default="")
    equipment = Column(String, default="")
    bukrs = Column(String, default="")
    lpc_text = Column(String, default="")

    observer_fullname = Column(String, default="")
    observer_perner = Column(String, default="")
    observer_position = Column(String, default="")
    observer_orgunit = Column(String, default="")
    observer_integration_name = Column(String, default="")

    observed_fullname = Column(String, default="")
    observed_perner = Column(String, default="")
    observed_position = Column(String, default="")
    observed_orgunit = Column(String, default="")
    observed_integration_name = Column(String, default="")

    location_key = Column(String, default="")
    location_name = Column(String, default="")
    location_text = Column(String, default="")

    created_on = Column(DateTime, default=now_utc)
    created_by = Column(String)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)
    changed_by = Column(String)


class ChecklistCheck(Base):
    __tablename__ = "checklist_check"
    __table_args__ = (Index("ix_checklist_check_root_id", "root_id"),)

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, ForeignKey("checklist_root.id"), nullable=False)
    text = Column(Text)
    comment = Column(Text)
    status = Column(String, default="PENDING")
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

    root = relationship("ChecklistRoot", back_populates="checks")


class ChecklistBarrier(Base):
    __tablename__ = "checklist_barrier"
    __table_args__ = (Index("ix_checklist_barrier_root_id", "root_id"),)

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, ForeignKey("checklist_root.id"), nullable=False)
    description = Column(Text)
    comment = Column(Text)
    is_active = Column(Boolean, default=True)
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

    root = relationship("ChecklistRoot", back_populates="barriers")


class AttachmentEntry(Base):
    __tablename__ = "attachment_entry"
    __table_args__ = (Index("ix_attachment_entry_root_id", "root_id"),)

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, ForeignKey("checklist_root.id"), nullable=False)
    folder_key = Column(String, default="")
    category_key = Column(String, default="GEN")
    file_name = Column(String, nullable=False)
    mime_type = Column(String, default="application/octet-stream")
    file_size = Column(INTEGER, default=0)
    storage_path = Column(String, nullable=False)
    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)


class LockEntry(Base):
    __tablename__ = "lock_entry"
    __table_args__ = (
        # Partial unique index: at most one *active* (non-killed) lock per db_key.
        # sqlite_where/postgresql_where are dialect-specific DDL options - SQLAlchemy
        # only applies the one matching the engine actually in use, so this stays a
        # correct partial index if DATABASE_URL is ever pointed at Postgres instead of
        # the default SQLite file (config.py already allows an arbitrary DATABASE_URL).
        Index(
            "ux_lock_entry_active_db_key",
            "db_key",
            unique=True,
            sqlite_where=text("is_killed = 0"),
            postgresql_where=text("is_killed = false"),
        ),
        # Plain (non-unique, non-partial) index for lookups that don't filter on
        # is_killed and so can't use the partial index above (e.g. lock history/audit).
        Index("ix_lock_entry_db_key", "db_key"),
    )

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    db_key = Column(String, nullable=False)
    user_id = Column(String, nullable=False)
    session_guid = Column(String, nullable=True)

    expires_at = Column(DateTime(timezone=True))
    locked_at = Column(DateTime(timezone=True))
    last_refresh_at = Column("last_heartbeat", DateTime(timezone=True))

    is_killed = Column(Boolean, default=False)
    killed_by = Column(String, nullable=True)


class LockLog(Base):
    __tablename__ = "lock_log"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    db_key = Column(String, nullable=False)
    user_id = Column(String)
    session_guid = Column(String, nullable=True)
    action = Column(String)
    timestamp = Column(DateTime, default=now_utc)


class AppUserProfile(Base):
    __tablename__ = "app_user_profile"

    uname = Column(String, primary_key=True)
    full_name = Column(String, nullable=False, default="")
    permissions_json = Column(Text, nullable=False, default="[]")
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)


class RuntimeUserContext(Base):
    __tablename__ = "runtime_user_context"

    key = Column(String, primary_key=True)
    uname = Column(String, nullable=False, default="")
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)


class SaveRequestLedger(Base):
    __tablename__ = "save_request_ledger"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    request_guid = Column(String, nullable=False)
    operation = Column(String, nullable=False)
    root_id = Column(String, nullable=False)
    user_id = Column(String, nullable=False)
    response_payload = Column(Text, nullable=False)
    created_on = Column(DateTime, default=now_utc)


class LastChangeSet(Base):
    __tablename__ = "last_change_set"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    entity_name = Column(String, nullable=False)
    entity_id = Column(String, nullable=True)
    last_change_timestamp = Column(DateTime, default=now_utc)


class Person(Base):
    __tablename__ = "persons"

    perner = Column(String, primary_key=True)
    first_name = Column(String)
    last_name = Column(String)
    middle_name = Column(String)
    position = Column(String)
    org_unit = Column(String)
    integration_name = Column(String)
    begda = Column(Date, nullable=False)
    endda = Column(Date, nullable=True)
    changed_on = Column(DateTime, default=now_utc)


class DictionaryItem(Base):
    __tablename__ = "dictionary_items"

    id = Column(String, primary_key=True)
    domain = Column(String, nullable=False)
    key = Column(String, nullable=False)
    text = Column(String, nullable=False)
    begda = Column(Date, nullable=False)
    endda = Column(Date, nullable=True)
    changed_on = Column(DateTime, default=now_utc)


class Location(Base):
    __tablename__ = "locations"

    node_id = Column(String, primary_key=True)
    parent_id = Column(String, ForeignKey("locations.node_id"), nullable=True)
    location_name = Column(String)

    begda = Column(Date, nullable=False)
    endda = Column(Date, nullable=True)

    changed_on = Column(DateTime, default=now_utc)


class AnalyticsSnapshot(Base):
    __tablename__ = "analytics_snapshot"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    month_key = Column(String, nullable=False)
    selected_year = Column(INTEGER, default=0)
    source_key = Column(String, default="ALL")
    available_years_json = Column(Text, default="[]")
    total_checklists = Column(INTEGER, default=0)
    month_checklists = Column(INTEGER, default=0)
    failed_checks = Column(INTEGER, default=0)
    failed_barriers = Column(INTEGER, default=0)
    failed_checklist_count = Column(INTEGER, default=0)
    failed_barrier_checklist_count = Column(INTEGER, default=0)
    closed_count = Column(INTEGER, default=0)
    registered_count = Column(INTEGER, default=0)
    avg_checks_rate = Column(Float, default=0)
    avg_barriers_rate = Column(Float, default=0)
    healthy_count = Column(INTEGER, default=0)
    source = Column(String, default="backend")
    refreshed_at = Column(DateTime, default=now_utc)
    created_on = Column(DateTime, default=now_utc)


class AnalyticsBreakdown(Base):
    __tablename__ = "analytics_breakdown"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    month_key = Column(String, nullable=False)
    selected_year = Column(INTEGER, default=0)
    source_key = Column(String, default="ALL")
    dimension = Column(String, nullable=False)
    metric = Column(String, nullable=False)
    bucket_key = Column(String, nullable=False)
    bucket_text = Column(String, nullable=False)
    metric_value = Column(INTEGER, default=0)
    sort_order = Column(INTEGER, default=0)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)


class AnalyticsRefreshState(Base):
    __tablename__ = "analytics_refresh_state"

    task_key = Column(String, primary_key=True)
    task_name = Column(String, nullable=False, default="")
    status = Column(String, nullable=False, default="IDLE")
    is_running = Column(Boolean, default=False)
    requested_at = Column(DateTime, nullable=True)
    requested_by = Column(String, default="")
    started_at = Column(DateTime, nullable=True)
    finished_at = Column(DateTime, nullable=True)
    last_success_at = Column(DateTime, nullable=True)
    last_error = Column(Text, default="")
    last_message = Column(Text, default="")
    active_run_id = Column(String, default="")
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)


class FrontendRuntimeSettings(Base):
    __tablename__ = "frontend_runtime_settings"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    # unique: bootstrap.ensure_runtime_settings_row() relies on this to detect - via
    # IntegrityError - a concurrent insert race for what's meant to be a singleton row
    # per environment. Without it, two racing callers that both see no existing row would
    # both succeed in inserting, silently producing duplicate settings rows.
    environment = Column(String, nullable=False, default="default", unique=True)
    heartbeat_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["heartbeat_ms"])
    lock_status_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["lock_status_ms"])
    gcd_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["gcd_ms"])
    idle_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["idle_ms"])
    autosave_interval_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["autosave_interval_ms"])
    autosave_debounce_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["autosave_debounce_ms"])
    lock_refresh_cooldown_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["lock_refresh_cooldown_ms"])
    network_grace_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["network_grace_ms"])
    cache_tolerance_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["cache_tolerance_ms"])
    analytics_refresh_ms = Column(INTEGER, default=FRONTEND_TIMER_PROFILE["analytics_refresh_ms"])
    required_fields_json = Column(Text, default="[]")
    upload_policy_json = Column(Text, default="{}")
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)
