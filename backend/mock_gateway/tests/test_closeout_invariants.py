import os
import sys
from pathlib import Path

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from main import app  # noqa: E402
from utils.odata import SERVICE_ROOT  # noqa: E402


REPO_ROOT = Path(__file__).resolve().parents[3]
APP_ROOT = REPO_ROOT / "app"
BACKEND_ROOT = REPO_ROOT / "backend" / "mock_gateway"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _csrf(client: TestClient) -> str:
    resp = client.get(f"{SERVICE_ROOT}/", headers={"X-CSRF-Token": "Fetch"})
    return str(resp.headers.get("X-CSRF-Token") or "")


def _search_rows(client: TestClient, top: int = 1, skip: int = 0) -> list[dict]:
    payload = client.get(f"{SERVICE_ROOT}/ChecklistSearchSet", params={"$top": top, "$skip": skip}).json()
    return payload.get("d", {}).get("results", [])


def test_legacy_modules_are_removed_from_active_repo_surface():
    removed_paths = [
        REPO_ROOT / "app" / "service" / "runtime" / "SmartCacheManager.js",
        REPO_ROOT / "app" / "util" / "SmartCacheStore.js",
        REPO_ROOT / "app" / "util" / "SmartCacheUtils.js",
        REPO_ROOT / "backend" / "mock_gateway" / "api" / "lock_api.py",
        REPO_ROOT / "backend" / "mock_gateway" / "api" / "actions_api.py",
        REPO_ROOT / "backend" / "mock_gateway" / "api" / "odata_compat_api.py",
        REPO_ROOT / "app" / "shell-user-runtime.js",
    ]

    for path in removed_paths:
        assert not path.exists(), f"legacy surface still present: {path}"


def test_active_frontend_code_has_no_forbidden_runtime_patterns():
    checked_suffixes = {".js", ".xml", ".html", ".json"}
    forbidden_patterns = {
        "requireSync": "sap.ui.requireSync",
        "legacyBusy": "/isBusy",
        "mockHeader": "X-Mock-User",
        "publicUname": "Uname",
    }

    for path in APP_ROOT.rglob("*"):
        if not path.is_file() or path.suffix.lower() not in checked_suffixes:
            continue
        text = _read(path)
        for label, pattern in forbidden_patterns.items():
            assert pattern not in text, f"{label} leaked into active frontend path: {path}"


def test_boot_and_runtime_source_lock_strict_success_path():
    boot_text = _read(APP_ROOT / "service" / "framework" / "ComponentBootRuntime.js")
    init_text = _read(APP_ROOT / "service" / "framework" / "ComponentInitRuntime.js")

    assert 'var bBootCompleted = false;' in boot_text
    assert 'resolveSettledStageError(aStageResults[0], "load_current_user_failed")' in boot_text
    assert 'resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed")' in boot_text
    assert 'resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed")' in boot_text
    assert 'if (bBootCompleted) {' in boot_text
    assert boot_text.index('if (bBootCompleted) {') < boot_text.index('oComponent._startCoreManagers();')
    assert 'ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {' in boot_text

    assert 'return runBootSequence({' in init_text
    assert 'throw oError || new Error("runtime_settings_load_failed");' in init_text
    assert 'return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh' in init_text


def test_open_detail_source_checks_permission_before_cache_and_backend_load():
    source = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "OpenDetailUseCase.js")

    non_create_branch = source[source.index("var oCacheValidation = mCtx && mCtx.cacheValidation;"):]
    permission_idx = non_create_branch.index("DetailAuthorizationSupport.fetchPermission")
    cache_idx = non_create_branch.index("oCacheValidation.execute")
    backend_idx = non_create_branch.index("oRepo.loadDetailSnapshot")

    assert permission_idx < cache_idx < backend_idx


def test_export_source_separates_selected_and_all_found_contracts():
    source = _read(APP_ROOT / "service" / "domain" / "search" / "usecases" / "ExportSearchUseCase.js")

    assert 'oRequest.rootIds = aSelectedIds;' in source
    assert 'oRequest.selectionMode = "selected";' in source
    assert 'oRequest.selectionMode = "all";' in source
    assert 'oRequest.searchContract = buildSearchContract(mInput, mCtx);' in source
    assert 'SearchMaxResults.resolveExportLimit(mState)' in source


def test_search_request_window_supports_legacy_search_max_results_shape():
    source = _read(APP_ROOT / "controller" / "support" / "SearchViewStateRuntime.js")
    util_source = _read(APP_ROOT / "util" / "search" / "SearchMaxResults.js")

    assert 'typeof SearchMaxResults.resolveGrowingPageSize === "function"' in source
    assert 'typeof SearchMaxResults.resolveMaxResults === "function"' in source
    assert 'resolveGrowingPageSize: resolveGrowingPageSize,' in util_source


def test_report_export_respects_selected_and_all_found_contracts():
    with TestClient(app) as client:
        token = _csrf(client)
        visible_rows = _search_rows(client, top=1, skip=0)
        hidden_rows = _search_rows(client, top=1, skip=1)

        assert visible_rows
        assert hidden_rows

        visible_row = visible_rows[0]
        hidden_row = hidden_rows[0]

        selected_resp = client.post(
            f"{SERVICE_ROOT}/ReportExport",
            json={
                "SelectionMode": "selected",
                "Entity": "screen",
                "RootKeys": [visible_row["Key"]],
                "Limit": 200000,
            },
            headers={"X-CSRF-Token": token},
        )
        assert selected_resp.status_code == 200
        selected_rows = selected_resp.json().get("d", {}).get("results", [])
        assert selected_rows
        assert {row.get("RootKey") for row in selected_rows} == {visible_row["Key"]}

        all_found_resp = client.post(
            f"{SERVICE_ROOT}/ReportExport",
            json={
                "SelectionMode": "all",
                "Entity": "screen",
                "SearchContract": {
                    "filterId": hidden_row["Id"],
                },
                "Limit": 200000,
            },
            headers={"X-CSRF-Token": token},
        )
        assert all_found_resp.status_code == 200
        all_found_rows = all_found_resp.json().get("d", {}).get("results", [])
        assert all_found_rows
        assert {row.get("Id") for row in all_found_rows} == {hidden_row["Id"]}
