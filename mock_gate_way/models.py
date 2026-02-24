# models.py

import uuid
from datetime import datetime, timedelta
from sqlalchemy import Column, String, DateTime, Boolean, Text
from sqlalchemy.dialects.sqlite import INTEGER
from database import Base


def now_utc():
    return datetime.utcnow()

location_key = Column(String, nullable=True)

class ChecklistRoot(Base):
    __tablename__ = "checklist_root"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    checklist_id = Column(String, nullable=False)
    lpc = Column(String, nullable=False)
    status = Column(String, default="01")

    created_on = Column(DateTime, default=now_utc)
    created_by = Column(String)

    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)
    changed_by = Column(String)

    is_deleted = Column(Boolean, default=False)


class LockEntry(Base):
    __tablename__ = "lock_entry"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    pcct_uuid = Column(String, nullable=False)
    user_id = Column(String, nullable=False)

    locked_at = Column(DateTime, default=now_utc)
    last_heartbeat = Column(DateTime, default=now_utc)
    expires_at = Column(DateTime)

    is_killed = Column(Boolean, default=False)


class LockLog(Base):
    __tablename__ = "lock_log"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    pcct_uuid = Column(String, nullable=False)
    user_id = Column(String)
    action = Column(String)  # ACQUIRE / RELEASE / STEAL / CLEANUP
    timestamp = Column(DateTime, default=now_utc)

class ChecklistCheck(Base):
    __tablename__ = "checklist_check"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, nullable=False)
    text = Column(Text)
    status = Column(String, default="PENDING")
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

class LastChangeSet(Base):
    __tablename__ = "last_change_set"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    entity_name = Column(String, nullable=False)
    entity_id = Column(String, nullable=True)
    last_change_timestamp = Column(DateTime, default=now_utc)

class ChecklistBarrier(Base):
    __tablename__ = "checklist_barrier"

    id = Column(String, primary_key=True, default=lambda: str(uuid.uuid4()))
    root_id = Column(String, nullable=False)
    description = Column(Text)
    is_active = Column(Boolean, default=True)
    position = Column(INTEGER)

    created_on = Column(DateTime, default=now_utc)
    changed_on = Column(DateTime, default=now_utc, onupdate=now_utc)

from datetime import date

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
    domain = Column(String, nullable=False)   # LPC / PROFESSION / etc
    key = Column(String, nullable=False)
    text = Column(String, nullable=False)

    begda = Column(Date, nullable=False)
    endda = Column(Date, nullable=True)

    changed_on = Column(DateTime, default=now_utc)

    class ChecklistRoot(Base):
    __tablename__ = "checklist_root"

    id = Column(String, primary_key=True)
    title = Column(String)
    lpc_level = Column(String)
    date_check = Column(Date)

    changed_on = Column(DateTime, default=now_utc)

    barriers = relationship(
        "Barrier",
        back_populates="root",
        cascade="all, delete-orphan"
    )

    checks = relationship(
        "Check",
        back_populates="root",
        cascade="all, delete-orphan"
    )

    class Barrier(Base):
    __tablename__ = "barriers"

    id = Column(String, primary_key=True)
    root_id = Column(String, ForeignKey("checklist_root.id"))

    name = Column(String)
    changed_on = Column(DateTime, default=now_utc)

    root = relationship("ChecklistRoot", back_populates="barriers")

    class Check(Base):
    __tablename__ = "checks"

    id = Column(String, primary_key=True)
    root_id = Column(String, ForeignKey("checklist_root.id"))

    description = Column(String)
    changed_on = Column(DateTime, default=now_utc)

    root = relationship("ChecklistRoot", back_populates="checks")