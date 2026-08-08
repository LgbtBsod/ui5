"""Database models and session management for SAP Gateway OData v2 mock.

Uses SQLAlchemy 2.0+ with type annotations and modern Python 3.14+ features.
All entity tables mirror the in-memory structure from state.py but persist to SQLite.

BOPF Draft Architecture:
- CheckRoots: ActiveUUID is the primary key (db_key), DraftUUID distinguishes instances
- Child entities (CheckBasics, CheckItems, Barriers): use parent_key = root's db_key
- This follows the real BOPF pattern where ActiveUUID = db_key for root entities
"""
from __future__ import annotations

import os
import json
from datetime import datetime, timezone
from typing import Any, Optional
from pathlib import Path
from uuid import uuid4

from sqlalchemy import (
    create_engine,
    Column,
    String,
    Boolean,
    Integer,
    DateTime,
    Text,
    ForeignKey,
    UniqueConstraint,
    Index,
    event,
)
from sqlalchemy.orm import (
    declarative_base,
    sessionmaker,
    relationship,
    Session,
    Mapped,
    mapped_column,
    validates,
)
from sqlalchemy.pool import StaticPool
from sqlalchemy.sql import func

# Import config for ZERO_GUID constant
import config

# Single base for all models
Base = declarative_base()


def generate_uuid() -> str:
    """Generate a UUID string compatible with SAP Gateway Edm.Guid."""
    return str(uuid4())


def odata_now() -> datetime:
    """Get current UTC datetime for OData timestamps."""
    return datetime.now(timezone.utc)


class CheckRoot(Base):
    """CheckRoots entity - main transactional root.
    
    BOPF Draft Pattern:
    - ActiveUUID (db_key): Identifies the business object instance
    - DraftUUID: Distinguishes between active (ZERO_GUID) and draft instances
    - IsActiveEntity: Boolean flag indicating if this is the active version
    - Composite primary key: (ActiveUUID, DraftUUID) allows multiple instances per business object
    """
    
    __tablename__ = "check_roots"
    
    # BOPF: Composite primary key (ActiveUUID, DraftUUID)
    # ActiveUUID identifies the business object, DraftUUID distinguishes instances
    ActiveUUID: Mapped[str] = mapped_column(String(36), primary_key=True)
    DraftUUID: Mapped[str] = mapped_column(String(36), primary_key=True, default=config.ZERO_GUID)
    RootId: Mapped[str] = mapped_column(String(36), default=generate_uuid, index=True)
    
    DocId: Mapped[str | None] = mapped_column(String(10), nullable=True)
    Status: Mapped[str | None] = mapped_column(String(50), nullable=True)
    ThisIsIntegrationData: Mapped[bool] = mapped_column(Boolean, default=False)
    
    # Draft fields (NW 7.50 / pre-S4 1610 pattern)
    IsActiveEntity: Mapped[bool] = mapped_column(Boolean, default=True)
    InProcessByUser: Mapped[str | None] = mapped_column(String(100), nullable=True)
    CreatedByUser: Mapped[str | None] = mapped_column(String(100), nullable=True)
    LastChangedByUser: Mapped[str | None] = mapped_column(String(100), nullable=True)
    CreatedAt: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True, default=odata_now)
    LastChangedAt: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True, onupdate=odata_now)
    
    # ETag support
    _etag_timestamp: Mapped[int] = mapped_column(Integer, default=0)
    
    # Navigation relationships - children use parent_key = ActiveUUID
    basic: Mapped[Optional["CheckBasic"]] = relationship(
        "CheckBasic", 
        back_populates="root", 
        uselist=False,
        cascade="all, delete-orphan",
        foreign_keys="CheckBasic.parent_key,CheckBasic.DraftUUID",
        primaryjoin="and_(CheckRoot.ActiveUUID == CheckBasic.parent_key, CheckRoot.DraftUUID == CheckBasic.DraftUUID)"
    )
    checks: Mapped[list["CheckItem"]] = relationship(
        "CheckItem",
        back_populates="root",
        cascade="all, delete-orphan",
        primaryjoin="and_(CheckRoot.ActiveUUID == CheckItem.parent_key, CheckRoot.DraftUUID == CheckItem.DraftUUID)"
    )
    barriers: Mapped[list["Barrier"]] = relationship(
        "Barrier",
        back_populates="root",
        cascade="all, delete-orphan",
        primaryjoin="and_(CheckRoot.ActiveUUID == Barrier.parent_key, CheckRoot.DraftUUID == Barrier.DraftUUID)"
    )
    
    __table_args__ = (
        Index("ix_check_roots_draft", "DraftUUID"),
        Index("ix_check_roots_active_entity", "IsActiveEntity"),
        Index("ix_check_roots_root_id", "RootId"),
    )


class CheckBasic(Base):
    """CheckBasics entity - 0..1 composition, proxy fields form.
    
    BOPF Draft Pattern:
    - parent_key = root's ActiveUUID (db_key)
    - DraftUUID matches the parent's DraftUUID for draft instances
    """
    
    __tablename__ = "check_basics"
    
    # BOPF: parent_key references root's ActiveUUID
    parent_key: Mapped[str] = mapped_column(String(36), ForeignKey("check_roots.ActiveUUID"), primary_key=True)
    DraftUUID: Mapped[str] = mapped_column(String(36), default=config.ZERO_GUID)
    
    Date: Mapped[str | None] = mapped_column(String(10), nullable=True)
    Time: Mapped[str | None] = mapped_column(String(8), nullable=True)
    Timezone: Mapped[str | None] = mapped_column(String(50), nullable=True)
    LocationKey: Mapped[str | None] = mapped_column(String(10), nullable=True)
    LocationName: Mapped[str | None] = mapped_column(String(100), nullable=True)
    Equipment: Mapped[str | None] = mapped_column(String(18), nullable=True)
    ObserverFullname: Mapped[str | None] = mapped_column(String(100), nullable=True)
    ObserverPerner: Mapped[str | None] = mapped_column(String(8), nullable=True)
    ObservedFullname: Mapped[str | None] = mapped_column(String(100), nullable=True)
    ObservedPerner: Mapped[str | None] = mapped_column(String(8), nullable=True)
    LpcKey: Mapped[str | None] = mapped_column(String(10), nullable=True)
    ProfKey: Mapped[str | None] = mapped_column(String(10), nullable=True)
    
    # Navigation - back to root via parent_key
    root: Mapped[CheckRoot] = relationship("CheckRoot", back_populates="basic")


class CheckItem(Base):
    """CheckItems entity - draft-enabled composition node.
    
    BOPF Draft Pattern:
    - parent_key = root's ActiveUUID (db_key)
    - DraftUUID distinguishes between active and draft instances
    - Composite unique key on (parent_key, ItemId, DraftUUID)
    """
    
    __tablename__ = "check_items"
    __table_args__ = (
        UniqueConstraint("parent_key", "ItemId", "DraftUUID", name="uq_check_items_parent_item_draft"),
        Index("ix_check_items_parent", "parent_key"),
        Index("ix_check_items_draft", "DraftUUID"),
    )
    
    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    # BOPF: parent_key references root's ActiveUUID
    parent_key: Mapped[str] = mapped_column(String(36), ForeignKey("check_roots.ActiveUUID"), nullable=False)
    ItemId: Mapped[str] = mapped_column(String(36), nullable=False)
    DraftUUID: Mapped[str] = mapped_column(String(36), default=config.ZERO_GUID)
    Question: Mapped[str | None] = mapped_column(Text, nullable=True)
    Result: Mapped[str | None] = mapped_column(String(1), nullable=True)
    Comment: Mapped[str | None] = mapped_column(Text, nullable=True)
    
    # Navigation - back to root via parent_key
    root: Mapped[CheckRoot] = relationship("CheckRoot", back_populates="checks")


class Barrier(Base):
    """Barriers entity - draft-enabled composition node.
    
    BOPF Draft Pattern:
    - parent_key = root's ActiveUUID (db_key)
    - DraftUUID distinguishes between active and draft instances
    - Composite unique key on (parent_key, BarrierId, DraftUUID)
    """
    
    __tablename__ = "barriers"
    __table_args__ = (
        UniqueConstraint("parent_key", "BarrierId", "DraftUUID", name="uq_barriers_parent_barrier_draft"),
        Index("ix_barriers_parent", "parent_key"),
        Index("ix_barriers_draft", "DraftUUID"),
    )
    
    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    # BOPF: parent_key references root's ActiveUUID
    parent_key: Mapped[str] = mapped_column(String(36), ForeignKey("check_roots.ActiveUUID"), nullable=False)
    BarrierId: Mapped[str] = mapped_column(String(36), nullable=False)
    DraftUUID: Mapped[str] = mapped_column(String(36), default=config.ZERO_GUID)
    BarrierType: Mapped[str | None] = mapped_column(String(50), nullable=True)
    Description: Mapped[str | None] = mapped_column(Text, nullable=True)
    Result: Mapped[str | None] = mapped_column(String(1), nullable=True)
    
    # Navigation - back to root via parent_key
    root: Mapped[CheckRoot] = relationship("CheckRoot", back_populates="barriers")


class DraftAdministrativeDatum(Base):
    """DraftAdministrativeData - virtual entity for lock info."""
    
    __tablename__ = "draft_admin_data"
    
    DraftUUID: Mapped[str] = mapped_column(String(36), primary_key=True)
    CreationDateTime: Mapped[datetime] = mapped_column(DateTime(timezone=True))
    CreationUser: Mapped[str] = mapped_column(String(100))
    LastChangeDateTime: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    LastChangeUser: Mapped[str | None] = mapped_column(String(100), nullable=True)
    DraftIsKeptByUser: Mapped[bool] = mapped_column(Boolean, default=False)
    EnqueueStartDateTime: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    DraftIsCreatedByMe: Mapped[bool] = mapped_column(Boolean, default=False)
    DraftIsLastChangedByMe: Mapped[bool] = mapped_column(Boolean, default=False)
    DraftIsProcessedByMe: Mapped[bool] = mapped_column(Boolean, default=False)
    InProcessByUser: Mapped[str | None] = mapped_column(String(100), nullable=True)
    InProcessByUserDescription: Mapped[str | None] = mapped_column(String(200), nullable=True)


def get_db_path() -> Path:
    """Get the database file path from environment or default location."""
    db_path = os.environ.get("ODOCK_MOCK_DB_PATH")
    if db_path:
        return Path(db_path)
    # Default: store in backend directory
    return Path(__file__).parent / "odata_mock.db"


def create_engine_session(db_path: Path | None = None, echo: bool = False) -> Session:
    """Create a SQLAlchemy engine and session factory.
    
    Args:
        db_path: Path to SQLite database file. Defaults to ODOCK_MOCK_DB_PATH env var
                 or backend/odata_mock.db
        echo: If True, log all SQL statements (for debugging)
    
    Returns:
        Configured SQLAlchemy Session instance
    """
    if db_path is None:
        db_path = get_db_path()
    
    # Ensure parent directory exists
    db_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Create engine with SQLite-specific optimizations
    engine = create_engine(
        f"sqlite:///{db_path}",
        echo=echo,
        poolclass=StaticPool,  # Single connection for single-threaded server
        connect_args={"check_same_thread": False},
    )
    
    # Create all tables
    Base.metadata.create_all(engine)
    
    # Create session factory
    SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False)
    
    return SessionLocal()


def init_db(db_path: Path | None = None, echo: bool = False) -> Session:
    """Initialize database and return session.
    
    Convenience wrapper that creates tables and returns a session.
    """
    return create_engine_session(db_path=db_path, echo=echo)
