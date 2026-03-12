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
        "gatewayIdentitySupport": "GatewayIdentitySupport",
        "withUserName": "withUserName",
        "resolveUserName": "resolveUserName",
        "smartCacheManager": "SmartCacheManager",
        "legacyDetailCurrent": "_detailCurrent",
        "legacyDetailSnapshot": "_detailSnapshot",
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
    feedback_bootstrap_text = _read(APP_ROOT / "service" / "framework" / "ComponentFeedbackBootstrapRuntime.js")
    boot_contracts = _read(APP_ROOT / "service" / "framework" / "ComponentBootContracts.js")
    feedback_contracts = _read(APP_ROOT / "service" / "framework" / "EffectFeedbackContracts.js")
    listener_contracts = _read(APP_ROOT / "service" / "framework" / "ComponentListenerContracts.js")
    save_guard_contracts = _read(APP_ROOT / "service" / "framework" / "ComponentSaveGuardContracts.js")
    cache_text = _read(APP_ROOT / "infra" / "adapters" / "BrowserCacheAdapter.js")
    listener_runtime = _read(APP_ROOT / "service" / "framework" / "ComponentListenerBindingRuntime.js")

    removed_framework_aliases = [
        APP_ROOT / "service" / "framework" / "ComponentLockRuntime.js",
        APP_ROOT / "service" / "framework" / "ComponentCoordinatorRuntime.js",
        APP_ROOT / "service" / "framework" / "ComponentLifecycleRuntime.js",
        APP_ROOT / "service" / "framework" / "ComponentRuntimeAttachmentBootstrap.js",
        APP_ROOT / "service" / "framework" / "ComponentListenerRuntime.js",
        APP_ROOT / "service" / "framework" / "ComponentListenerStateRuntime.js",
        APP_ROOT / "service" / "framework" / "ComponentInitAttachmentStageRuntime.js",
        APP_ROOT / "service" / "framework" / "FrontendConfigConstants.js",
        APP_ROOT / "service" / "framework" / "ComponentRuntimeSupport.js",
    ]

    assert 'var bBootCompleted = false;' in boot_text
    assert 'resolveSettledStageError(aStageResults[0], "load_current_user_failed")' in boot_text
    assert 'resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed")' in boot_text
    assert 'resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed")' in boot_text
    assert 'cleanupStaleSessions' in boot_text
    assert 'if (bBootCompleted) {' in boot_text
    assert boot_text.index('if (bBootCompleted) {') < boot_text.index('oComponent._startCoreManagers();')
    assert 'ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {' in boot_text

    assert 'return ComponentBootRuntime.runBootSequence({' in init_text
    assert 'cacheAdapter: this._ctx && this._ctx.cache,' in init_text
    assert 'throw oError || new Error("runtime_settings_load_failed");' in feedback_bootstrap_text
    assert 'return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh' in init_text
    assert 'READINESS_STATUS' in boot_contracts
    assert 'STAGE_ERRORS' in boot_contracts
    assert 'TOAST_SHOW_MS' in feedback_contracts
    assert 'WORKFLOW_MODE_CHANGED' in listener_contracts
    assert 'GUARDED_FAILED' in save_guard_contracts

    assert 'clearCurrentTab: function () {' in cache_text
    assert 'clearByTabSessionId: clearByTabSessionId,' in cache_text
    assert 'cleanupStaleSessions: cleanupStaleSessions,' in cache_text
    assert 'StatePaths.UI_BUSY_GLOBAL' not in listener_runtime
    for path in removed_framework_aliases:
        assert not path.exists(), f"alias-only framework file still present: {path}"


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
    assert 'filterLocationKey:' in source


def test_create_permission_contract_is_current_identity_only():
    source = _read(APP_ROOT / "infra" / "adapters" / "ODataChecklistRepoAdapter.js")
    readme = _read(BACKEND_ROOT / "README_ODATA.md")

    assert "ChecklistCreatePermissionSet('CURRENT')" in source
    assert "Productive create-permission seam rules:" in readme
    assert "response entity identity also stays `RootKey='CURRENT'`" in readme
    assert "normalizePermissionResponse()" in readme


def test_local_validation_guide_matches_real_repo_assets():
    guide_path = REPO_ROOT / "docs" / "LOCAL_VALIDATION.md"
    guide = _read(guide_path)

    assert guide_path.exists()
    assert "scripts/start-local-env.ps1" in guide
    assert "scripts/stop-local-env.ps1" in guide
    assert "node scripts/gateway-live-smoke-runner.js" in guide
    assert "Manual smoke playbook" in guide
    assert "EDIT_LOCKED" in guide
    assert "filterLocationKey" in guide


def test_start_local_env_supports_python_fallback_and_external_gateway_mode():
    source = _read(REPO_ROOT / "scripts" / "start-local-env.ps1")

    assert "PYTHON_BIN" in source
    assert "py -3 launcher" in source
    assert "python on PATH" in source
    assert "FastAPI and uvicorn" in source
    assert "-GatewayBaseUrl" in source or "GatewayBaseUrl" in source


def test_telemetry_uses_canonical_lock_state_vocabulary():
    telemetry_runtime = _read(APP_ROOT / "service" / "framework" / "TelemetryRuntime.js")
    workflow_telemetry = _read(APP_ROOT / "util" / "WorkflowTelemetry.js")
    lock_gate = _read(REPO_ROOT / "scripts" / "gates" / "lock-state-enum-gate.js")

    assert "lockState:" in telemetry_runtime
    assert "lockOperationState:" not in telemetry_runtime
    assert "lockState:" in workflow_telemetry
    assert "lockOperationState:" not in workflow_telemetry
    assert "lockState" in lock_gate


def test_search_request_window_supports_legacy_search_max_results_shape():
    source = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewStateRuntime.js")
    util_source = _read(APP_ROOT / "util" / "search" / "SearchMaxResults.js")

    assert 'typeof SearchMaxResults.resolveGrowingPageSize === "function"' in source
    assert 'typeof SearchMaxResults.resolveMaxResults === "function"' in source
    assert 'resolveGrowingPageSize: resolveGrowingPageSize,' in util_source


def test_sticky_runtime_is_route_scoped_without_global_body_observer():
    legacy_path = APP_ROOT / "search-toolbar-sticky-runtime.js"
    source = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewportRuntime.js")

    assert not legacy_path.exists()
    assert "MutationObserver" not in source
    assert 'new window.ResizeObserver(function () {' in source
    assert 'window.addEventListener("resize", oController._fnSearchViewportResize);' in source
    assert 'window.removeEventListener("resize", oController._fnSearchViewportResize);' in source
    assert 'oScrollHost.addEventListener("scroll", oController._fnSearchScrollSync, { passive: true });' in source
    assert 'oObserver.disconnect();' in source


def test_detail_meta_bucket_is_explicit_and_synced_from_canonical_state():
    state_paths = _read(APP_ROOT / "model" / "StatePaths.js")
    workflow_schema = _read(APP_ROOT / "model" / "schema" / "workflowSchema.js")
    listener_runtime = _read(APP_ROOT / "service" / "framework" / "ComponentDetailMetaSyncRuntime.js")

    assert 'DETAIL_META: "/detailMeta",' in state_paths
    assert 'detailMeta:' in workflow_schema
    assert 'ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {' in listener_runtime


def test_standardized_telemetry_event_names_cover_permission_cache_lock_and_analytics_flows():
    permission_source = _read(APP_ROOT / "service" / "domain" / "detail" / "DetailAuthorizationSupport.js")
    cache_source = _read(APP_ROOT / "service" / "domain" / "cache" / "usecases" / "CacheValidationUseCase.js")
    enter_edit_source = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "EnterEditUseCase.js")
    close_detail_source = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "CloseDetailUseCase.js")
    search_analytics_source = _read(APP_ROOT / "service" / "domain" / "search" / "usecases" / "AnalyticsUseCase.js")
    dashboard_analytics_source = _read(APP_ROOT / "service" / "domain" / "analytics" / "usecases" / "LoadAnalyticsDashboardUseCase.js")

    assert 'permission.denied' in permission_source
    assert 'cache.hit' in cache_source
    assert 'cache.miss' in cache_source
    assert 'cache.invalidated' in cache_source
    assert 'lock.acquire.success' in enter_edit_source
    assert 'lock.acquire.failed' in enter_edit_source
    assert 'lock.release.completed' in enter_edit_source
    assert 'lock.release.completed' in close_detail_source
    assert 'analytics.search.loaded' in search_analytics_source
    assert 'analytics.search.error' in search_analytics_source
    assert 'analytics.dashboard.loaded' in dashboard_analytics_source
    assert 'analytics.dashboard.error' in dashboard_analytics_source


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
                    "filterLocationKey": hidden_row["LocationKey"],
                },
                "Limit": 200000,
            },
            headers={"X-CSRF-Token": token},
        )
        assert all_found_resp.status_code == 200
        all_found_rows = all_found_resp.json().get("d", {}).get("results", [])
        assert all_found_rows
        assert {row.get("Id") for row in all_found_rows} == {hidden_row["Id"]}
        assert {row.get("LocationKey") for row in all_found_rows} == {hidden_row["LocationKey"]}
