"""
Pytest configuration and shared test fixtures.
Consolidates repeated test helper functions.
"""
import pytest
from fastapi.testclient import TestClient
from datetime import datetime, timezone
from services.db_seed import seed_persons, seed_locations, reset_db


@pytest.fixture
def client():
    """Provides FastAPI test client."""
    from main import app
    return TestClient(app)


def get_csrf_token(client: TestClient) -> str:
    """Get CSRF token from frontend settings endpoint."""
    response = client.get("/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/FrontendSettingsSet")
    if response.status_code == 200:
        data = response.json()
        result = data.get("d", {})
        if isinstance(result, dict):
            csrf = result.get("CsrfToken")
            if csrf:
                return str(csrf)
    return ""


def create_sample_root(client: TestClient) -> dict:
    """Create sample checklist root for testing."""
    csrf = get_csrf_token(client)
    payload = {
        "checklist_id": "TEST_2024_001",
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
    if response.status_code == 201:
        return response.json().get("d", {})
    return {}


# Legacy fixture functions for backwards compatibility in existing tests
def _csrf(client: TestClient) -> str:
    """Backwards-compatible wrapper for get_csrf_token."""
    return get_csrf_token(client)


def _sample_root(client: TestClient) -> dict:
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
    return response.json().get("d", {}) if response.status_code == 201 else {}


# Legacy wrapper
def _create_root(client: TestClient) -> dict:
    """Backwards-compatible wrapper for create_test_root."""
    return create_test_root(client)
