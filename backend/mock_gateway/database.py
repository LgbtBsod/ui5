from contextlib import contextmanager
from contextvars import ContextVar

from sqlalchemy import create_engine, event
from sqlalchemy.orm import declarative_base, sessionmaker

from config import DATABASE_URL

engine = create_engine(DATABASE_URL, connect_args={"check_same_thread": False})

if engine.dialect.name == "sqlite":
    # Standard SQLAlchemy recipe for pysqlite: its own implicit transaction handling
    # otherwise interferes with SAVEPOINT support (Session.begin_nested()), which
    # api/batch_api.py needs for atomic $batch changesets. Without this, pysqlite
    # silently lets a nested transaction's commit end the *entire* transaction instead
    # of just the savepoint.
    @event.listens_for(engine, "connect")
    def _sqlite_disable_pysqlite_transaction_handling(dbapi_connection, connection_record):
        dbapi_connection.isolation_level = None
        # SQLite is single-writer: two connections (e.g. the periodic analytics refresh
        # job and a concurrent request) committing at the same moment otherwise raise
        # "database is locked" immediately. Worse, since isolation_level=None hands
        # transaction control to us, a commit that fails this way leaves the SQLite-level
        # transaction still open on that pooled connection - the next checkout's explicit
        # BEGIN (below) then fails with "cannot start a transaction within a transaction",
        # poisoning that connection for every future request that happens to reuse it.
        # A busy_timeout makes SQLite retry internally for up to 30s instead of failing
        # immediately, which is orders of magnitude longer than any transaction in this
        # app takes - eliminating the lock error (and the corruption it causes) in practice.
        dbapi_connection.execute("PRAGMA busy_timeout = 30000")

    @event.listens_for(engine, "begin")
    def _sqlite_emit_explicit_begin(conn):
        conn.exec_driver_sql("BEGIN")

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()

# Lets a $batch changeset share one Session/transaction across its sub-operations (see
# api/batch_api.py) instead of each nested ASGI sub-request getting its own get_db()
# session that commits independently - which made a changeset unable to roll back
# earlier successful operations when a later one in the same changeset failed.
# ContextVar (not a plain module global) so concurrent requests don't see each other's
# shared session; it propagates correctly through httpx.ASGITransport's in-process,
# same-task dispatch of the nested sub-request.
_shared_session: ContextVar = ContextVar("_shared_session", default=None)


@contextmanager
def use_shared_session(session):
    token = _shared_session.set(session)
    try:
        yield session
    finally:
        _shared_session.reset(token)


def get_db():
    shared = _shared_session.get()
    if shared is not None:
        yield shared
        return
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()
