from sqlalchemy import create_engine, inspect

import main
from database import Base, SessionLocal


def test_ensure_schema_compatibility_creates_missing_analytics_tables(monkeypatch):
    engine = create_engine("sqlite:///:memory:", connect_args={"check_same_thread": False})
    Base.metadata.create_all(
        bind=engine,
        tables=[
            main.ChecklistRoot.__table__,
            main.ChecklistCheck.__table__,
            main.ChecklistBarrier.__table__,
        ]
    )

    monkeypatch.setattr(main, "engine", engine)
    main.ensure_schema_compatibility()

    inspector = inspect(engine)
    table_names = set(inspector.get_table_names())
    assert "analytics_snapshot" in table_names
    assert "analytics_breakdown" in table_names
    assert "analytics_refresh_state" in table_names
    assert "frontend_runtime_settings" in table_names


def test_ensure_runtime_settings_row_backfills_missing_scheduler_row(monkeypatch):
    engine = create_engine("sqlite:///:memory:", connect_args={"check_same_thread": False})
    Base.metadata.create_all(bind=engine)

    monkeypatch.setattr(main, "engine", engine)
    main.ensure_schema_compatibility()

    TestingSession = SessionLocal
    monkeypatch.setattr(main, "SessionLocal", lambda: TestingSession(bind=engine))
    db = main.SessionLocal()
    try:
        row = main._ensure_runtime_settings_row(db)
        assert row is not None
        assert row.analytics_refresh_ms is not None
        assert row.required_fields_json
        assert row.upload_policy_json
    finally:
        db.close()
