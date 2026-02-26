import uuid

from sqlalchemy import Boolean, Column, Date, DateTime, ForeignKey, String, Text
from sqlalchemy.dialects.sqlite import INTEGER
from sqlalchemy.orm import relationship

from database import Base
from utils.time import now_utc


class ChecklistRoot(Base):
    __tablename__ = "checklist_root"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    checklist_id = Column(String, nullable=False)
    lpc = Column(String, nullable=False)
    status = Column(String, default="01")

    date = Column(String, default="")
    equipment = Column(String, default="")
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


class ChecklistCheck(Base):
    __tablename__ = "checklist_check"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, ForeignKey("checklist_root.id"), nullable=False)
    text = Column(Text)
    status = Column(String, default="PENDING")
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

    root = relationship("ChecklistRoot", back_populates="checks")


class ChecklistBarrier(Base):
    __tablename__ = "checklist_barrier"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, ForeignKey("checklist_root.id"), nullable=False)
    description = Column(Text)
    is_active = Column(Boolean, default=True)
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

    root = relationship("ChecklistRoot", back_populates="barriers")


class LockEntry(Base):
    __tablename__ = "lock_entry"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    pcct_uuid = Column(String, nullable=False)
    user_id = Column(String, nullable=False)
    session_guid = Column(String, nullable=True)

    expires_at = Column(DateTime(timezone=True))
    locked_at = Column(DateTime(timezone=True))
    last_heartbeat = Column(DateTime(timezone=True))

    is_killed = Column(Boolean, default=False)
    killed_by = Column(String, nullable=True)


class LockLog(Base):
    __tablename__ = "lock_log"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    pcct_uuid = Column(String, nullable=False)
    user_id = Column(String)
    session_guid = Column(String, nullable=True)
    action = Column(String)
    timestamp = Column(DateTime, default=now_utc)


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
    total_checklists = Column(INTEGER, default=0)
    month_checklists = Column(INTEGER, default=0)
    failed_checks = Column(INTEGER, default=0)
    failed_barriers = Column(INTEGER, default=0)
    created_on = Column(DateTime, default=now_utc)
