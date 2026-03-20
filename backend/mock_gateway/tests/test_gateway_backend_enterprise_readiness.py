from __future__ import annotations

import os
import sys

from fastapi.testclient import TestClient
from sqlalchemy.exc import OperationalError

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from main import app  # noqa: E402
from config import ALLOW_MOCK_USER_HEADER, APP_PROFILE, AUTO_MUTATE_SCHEMA_ON_STARTUP, AUTO_SEED_STARTUP_DATA  # noqa: E402
from services.current_user_service import CurrentUserService  # noqa: E402
from utils.odata import SERVICE_ROOT  # noqa: E402


def test_capabilities_paths_are_gateway_canonical():
    with TestClient(app) as client:
        payload = client.get(f"{SERVICE_ROOT}/capabilities").json()
    paths = (payload or {}).get('paths') or {}
    assert paths.get('serviceRoot') == SERVICE_ROOT
    assert paths.get('saveFunctionImport') == '/SaveChanges'
    assert paths.get('autosaveFunctionImport') == '/AutoSave'
    assert paths.get('lockStatusEntity') == '/LockStatusSet'
    assert paths.get('lockHistoryEntity') == '/LockLogs'


def test_capabilities_identity_mode_matches_backend_flag():
    with TestClient(app) as client:
        payload = client.get(f"{SERVICE_ROOT}/capabilities").json()
    assert payload.get('identityMode') == ('mock_header' if ALLOW_MOCK_USER_HEADER else 'runtime_user_context')


def test_capabilities_expose_profile_and_startup_mutation_policy():
    with TestClient(app) as client:
        payload = client.get(f"{SERVICE_ROOT}/capabilities").json()
    assert payload.get('profile') == APP_PROFILE
    startup_mutation = payload.get('startupMutation') or {}
    assert startup_mutation.get('schema') is AUTO_MUTATE_SCHEMA_ON_STARTUP
    assert startup_mutation.get('seedData') is AUTO_SEED_STARTUP_DATA


def test_unprefixed_analytics_route_is_present_for_compatibility_mode():
    with TestClient(app) as client:
        resp = client.get('/SimpleAnalytical')
    assert resp.status_code == 200


def test_canonical_prefixed_routes_remain_available():
    with TestClient(app) as client:
        resp = client.get(f"{SERVICE_ROOT}/SimpleAnalytical")
    assert resp.status_code == 200


def test_current_user_service_falls_back_when_runtime_identity_tables_are_missing():
    class BrokenDb:
        def get(self, _entity, _key):
            raise OperationalError("SELECT 1", {}, Exception("missing table"))

    uname = CurrentUserService.resolve_uname(db=BrokenDb())
    profile = CurrentUserService.resolve_profile(db=BrokenDb())

    assert uname == "operator"
    assert profile == {
        "uname": "operator",
        "full_name": "operator",
        "permissions": [],
    }
