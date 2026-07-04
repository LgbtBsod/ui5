"""
Pytest configuration and shared test fixtures.
Consolidates repeated test helper functions.
"""
import pytest
from fastapi.testclient import TestClient
from datetime import datetime, timezone


@pytest.fixture
def client():
    """Provides FastAPI test client."""
    from main import app
    return TestClient(app)


@pytest.fixture(autouse=True)
def _reset_rate_limiter():
    """The rate limiter is a single in-process instance shared by every TestClient(app)
    across the whole pytest session (app is a module-level singleton). Every TestClient
    request uses the same "testclient" host, so without a reset between tests, request
    counts accumulate across unrelated test files until the window trips 429 on an
    otherwise-unrelated test - not a real rate-limit violation, just shared test state."""
    from main import app
    limiter = getattr(app.state, "rate_limiter", None)
    if limiter is not None:
        limiter.reset_all()
    yield


def get_csrf_token(client: TestClient) -> str:
    """Fetch a CSRF token via the real SAP CSRF handshake (GET + X-CSRF-Token: fetch),
    as implemented by odata_csrf_middleware. The token is returned in the response
    header, not the response body; the session cookie is stored on the client."""
    response = client.get(
        "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/$metadata",
        headers={"X-CSRF-Token": "fetch"},
    )
    return response.headers.get("X-CSRF-Token", "")


def create_sample_root(client: TestClient) -> str:
    """Create sample checklist root for testing; returns its DB_KEY."""
    csrf = get_csrf_token(client)
    payload = {
        "checklist_id": "TEST_2024_001",
        "lpc": "LPC01",
        "observed_position": "POS01",
        "location_key": "LOC01",
        "bukrs": "BUKR",
        "status": "DRAFT",
        # PascalCase: consumed by _normalize_basic_payload(), not the snake_case keys above.
        "Date": datetime.now(timezone.utc).date().isoformat(),
        "Equipment": "Pump Test A1",
    }
    headers = {"X-CSRF-Token": csrf} if csrf else {}
    response = client.post(
        "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistRootSet",
        json=payload,
        headers=headers
    )
    if response.status_code in (200, 201):
        return response.json().get("d", {}).get("DB_KEY", "")
    return ""


# Legacy fixture functions for backwards compatibility in existing tests
def _csrf(client: TestClient) -> str:
    """Backwards-compatible wrapper for get_csrf_token."""
    return get_csrf_token(client)


def _sample_root(client: TestClient) -> str:
    """Backwards-compatible wrapper for create_sample_root."""
    return create_sample_root(client)


def create_test_root(client: TestClient) -> dict:
    """Create a test root for testing (consolidates _create_root)."""
    csrf = get_csrf_token(client)
    payload = {
        "checklist_id": f"TEST_{datetime.now().timestamp()}",
        "lpc": "LPC01",
        "observed_position": "POS01",
        "location_key": "LOC01",
        "bukrs": "BUKR",
        "status": "DRAFT",
    }
    headers = {"X-CSRF-Token": csrf} if csrf else {}
    response = client.post(
        "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistRootSet",
        json=payload,
        headers=headers
    )
    return response.json().get("d", {}) if response.status_code in (200, 201) else {}


# Legacy wrapper
def _create_root(client: TestClient) -> dict:
    """Backwards-compatible wrapper for create_test_root."""
    return create_test_root(client)
