import os
import sys

import pytest

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from database import SessionLocal  # noqa: E402
from database import Base, engine  # noqa: E402
from models import ChecklistRoot, SaveRequestLedger  # noqa: E402
from api.search_api import search_rows  # noqa: E402
from services import checklist_service  # noqa: E402
from services.checklist_service import ChecklistService  # noqa: E402
from services.db_seed import seed_reference_data  # noqa: E402
from main import _seed_checklist_roots_if_needed  # noqa: E402


@pytest.fixture(autouse=True)
def reset_db():
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)
    with SessionLocal() as db:
        seed_reference_data(db)
        _seed_checklist_roots_if_needed(db, minimum_rows=5)
    yield
    Base.metadata.drop_all(bind=engine)


def test_search_rows_clamps_invalid_window_and_skips_inlinecount_by_default():
    db = SessionLocal()
    try:
        payload = search_rows(filter=None, top=999999, skip=-20, inlinecount=None, db=db).get("d", {})
        rows = payload.get("results", [])

        assert rows
        assert len(rows) <= 200
        assert "__count" not in payload
    finally:
        db.close()


def test_save_via_import_rejects_corrupt_request_ledger(monkeypatch):
    db = SessionLocal()
    try:
        root = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).first()
        assert root is not None

        ledger = SaveRequestLedger(
            request_guid="REQ-CORRUPT-LEDGER",
            operation="SAVE",
            root_id=root.id,
            user_id="ANON",
            response_payload="{broken-json",
        )
        db.add(ledger)
        db.commit()

        monkeypatch.setattr(checklist_service.LockService, "validate_lock", staticmethod(lambda *_args, **_kwargs: None))
        monkeypatch.setattr(checklist_service.LockService, "validate_session_lock", staticmethod(lambda *_args, **_kwargs: None))

        with pytest.raises(ValueError, match="REQUEST_LEDGER_CORRUPT"):
            ChecklistService.save_via_import(
                db,
                root.id,
                "ANON",
                {"checks": [], "barriers": []},
                request_guid="REQ-CORRUPT-LEDGER",
            )
    finally:
        db.query(SaveRequestLedger).filter(SaveRequestLedger.request_guid == "REQ-CORRUPT-LEDGER").delete()
        db.commit()
        db.close()
