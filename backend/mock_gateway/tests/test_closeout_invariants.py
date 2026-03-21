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
        REPO_ROOT / "app" / "contracts" / "AnalyticsContracts.js",
        REPO_ROOT / "app" / "contracts" / "DialogContracts.js",
        REPO_ROOT / "app" / "contracts" / "FrontendConfigConstants.js",
        REPO_ROOT / "app" / "contracts" / "ModelContracts.js",
        REPO_ROOT / "app" / "contracts" / "NavigationContracts.js",
        REPO_ROOT / "app" / "contracts" / "OperationSourceContracts.js",
        REPO_ROOT / "app" / "contracts" / "SearchRuntimeContracts.js",
        REPO_ROOT / "app" / "contracts" / "SearchUiContracts.js",
        REPO_ROOT / "app" / "contracts" / "ShellPaneContracts.js",
        REPO_ROOT / "app" / "contracts" / "WorkflowContracts.js",
        REPO_ROOT / "app" / "contracts" / "AnalyticsUiContracts.js",
        REPO_ROOT / "app" / "contracts" / "DetailRuntimeContracts.js",
        REPO_ROOT / "app" / "contracts" / "ProgressiveReadinessContracts.js",
        REPO_ROOT / "app" / "contracts" / "ReadinessTelemetryContracts.js",
        REPO_ROOT / "app" / "contracts" / "UiAssetPaths.js",
        REPO_ROOT / "app" / "constants" / "AppConstants.js",
        REPO_ROOT / "app" / "service" / "framework" / "UseCase.js",
        REPO_ROOT / "app" / "service" / "shared" / "ExcelExport.js",
        REPO_ROOT / "app" / "service" / "domain" / "search" / "ExportFacade.js",
        REPO_ROOT / "app" / "service" / "domain" / "shared" / "LockFacade.js",
        REPO_ROOT / "app" / "service" / "shared" / "delta" / "DeltaDateCodec.js",
        REPO_ROOT / "app" / "service" / "shared" / "delta" / "DeltaChildChanges.js",
        REPO_ROOT / "app" / "service" / "runtime" / "ManagerFacade.js",
        REPO_ROOT / "app" / "service" / "framework" / "FacadeCommandRuntime.js",
        REPO_ROOT / "app" / "infra" / "navigation" / "RouteSync.js",
        REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_lock_manager.clas.abap",
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
        "legacyUseCaseBaseImport": "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
        "legacyUseCasePrototype": "Object.create(UseCase.prototype)",
        "legacyUseCaseCtorCall": "UseCase.call(this",
        "legacyExcelExportWrapper": "PRODUCTION_CONTROL_CHECKLIST/service/shared/ExcelExport",
        "legacyExportFacade": "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/ExportFacade",
        "legacyCreateAlias": "__CREATE__",
        "legacyLayoutStorageKey": "sap_ui5_layout_personalization",
        "legacyThemeProfileKey": "sap_ui5_theme_profile",
        "legacyThemeKey": "sap_ui5_theme",
        "lockFacade": "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/LockFacade",
        "deltaDateCodec": "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaDateCodec",
        "deltaChildChanges": "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaChildChanges",
        "managerFacade": "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ManagerFacade",
        "facadeCommandRuntime": "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    }

    for path in APP_ROOT.rglob("*"):
        if not path.is_file() or path.suffix.lower() not in checked_suffixes:
            continue
        text = _read(path)
        for label, pattern in forbidden_patterns.items():
            assert pattern not in text, f"{label} leaked into active frontend path: {path}"


def test_productive_frontend_uses_canonical_constant_modules_without_contract_wrappers():
    wrapper_import_markers = [
        "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/DialogContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsUiContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/DetailRuntimeContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/FrontendConfigConstants",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/ProgressiveReadinessContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/ReadinessTelemetryContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchRuntimeContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchUiContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/ShellPaneContracts",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/UiAssetPaths",
        "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    ]

    for path in APP_ROOT.rglob("*.js"):
        text = _read(path)
        for marker in wrapper_import_markers:
            assert marker not in text, f"contract wrapper import leaked into active frontend path: {path}"


def test_productive_frontend_has_no_app_constants_god_object_imports():
    forbidden_markers = [
        "PRODUCTION_CONTROL_CHECKLIST/constants/AppConstants",
        "constants/AppConstants",
    ]

    for path in APP_ROOT.rglob("*"):
        if not path.is_file():
            continue
        if path.suffix.lower() not in {".js", ".xml", ".json", ".html"}:
            continue
        text = _read(path)
        for marker in forbidden_markers:
            assert marker not in text, f"AppConstants aggregator import leaked into repo path: {path}"


def test_manifest_and_model_constants_expose_only_current_global_model_surface():
    manifest = _read(APP_ROOT / "manifest.json")
    model_constants = _read(APP_ROOT / "constants" / "ModelConstants.js")
    model_bootstrap = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentModelInitRuntime.js")
    model_factory = _read(APP_ROOT / "model" / "ModelFactory.js")
    app_view = _read(APP_ROOT / "views" / "App.view.xml")

    assert '"shell": {' in manifest
    assert '"detail": {' in manifest
    assert '"state": {' in manifest
    assert '"masterData": {' in manifest
    assert '"cache": {' not in manifest
    assert '"selected": {' not in manifest
    assert '"locationtree": {' not in manifest
    assert '"data": {' not in manifest
    assert '"uiState": {' not in manifest
    assert '"layout": {' not in manifest
    assert '"mpl": {' not in manifest
    assert '"appView": {' not in manifest

    assert 'DATA: "data"' not in model_constants
    assert 'CACHE: "cache"' not in model_constants
    assert 'ENV: "env"' not in model_constants
    assert 'LOCATION_TREE: "locationtree"' not in model_constants
    assert 'DETAIL: "detail"' in model_constants
    assert 'SHELL: "shell"' in model_constants
    assert 'SHELL_LAYOUT: "/layout"' in model_constants

    assert 'MODELS.DATA' not in model_bootstrap
    assert 'createDataModel' not in model_bootstrap
    assert 'MODELS.LOCATION_TREE' not in model_bootstrap
    assert 'setModel(mModels.cacheModel' not in model_bootstrap
    assert 'setModel(mModels.envModel' not in model_bootstrap
    assert '_internalRuntimeModels' not in model_bootstrap
    assert 'createInternalCacheState' not in model_bootstrap
    assert 'createInternalEnvState' not in model_bootstrap
    assert '_runtimeModels' not in model_bootstrap
    assert 'createCacheModel' not in model_factory
    assert 'createEnvModel' not in model_factory
    assert 'createHierarchyModel' not in model_factory
    assert 'layout="{= ${shell>/layout} || \'OneColumn\' }"' in app_view


def test_boot_and_runtime_source_lock_strict_success_path():
    bootstrap_source = _read(APP_ROOT / "service" / "framework" / "ComponentBootstrap.js")
    bootstrap_builder = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentBootstrapDependencyBuilder.js")
    backend_mode_contracts = _read(APP_ROOT / "service" / "domain" / "shared" / "BackendModeContracts.js")
    boot_text = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentBootRuntime.js")
    init_text = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentCoreInitRuntime.js")
    feedback_bootstrap_text = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentFeedbackInitRuntime.js")
    boot_contracts = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentBootContracts.js")
    feedback_contracts = _read(APP_ROOT / "service" / "framework" / "EffectFeedbackContracts.js")
    listener_contracts = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentListenerContracts.js")
    save_guard_contracts = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentSaveGuardContracts.js")
    cache_text = _read(APP_ROOT / "infra" / "adapters" / "BrowserCacheAdapter.js")
    listener_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentListenerBindingRuntime.js")

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
    assert "resolveSettledStageError(aStageResults[0], STAGE_ERRORS.LOAD_CURRENT_USER_FAILED)" in boot_text
    assert "resolveSettledStageError(aStageResults[1], STAGE_ERRORS.LOAD_RUNTIME_SETTINGS_FAILED)" in boot_text
    assert "resolveSettledStageError(aStageResults[2], STAGE_ERRORS.BOOTSTRAP_INIT_BUNDLE_FAILED)" in boot_text
    assert "cleanupStaleSessions" in boot_text
    assert 'if (bBootCompleted) {' in boot_text
    assert boot_text.index('if (bBootCompleted) {') < boot_text.index('oComponent._startCoreManagers();')
    assert 'ModelStateRuntime.writeOnModel(oStateModel, PATHS.READINESS_APP, {' in boot_text

    assert "initializeComponentRuntime" in init_text
    assert "buildLatestCtx" in init_text
    assert "resolveDetailCurrent" in init_text
    assert 'throw oError || new Error("runtime_settings_load_failed");' in feedback_bootstrap_text
    assert 'READINESS_STATUS' in boot_contracts
    assert 'STAGE_ERRORS' in boot_contracts
    assert 'TOAST_SHOW_MS' in feedback_contracts
    assert 'WORKFLOW_MODE_CHANGED' in listener_contracts
    assert 'GUARDED_FAILED' in save_guard_contracts

    assert 'clearCurrentTab: function () {' in cache_text
    assert 'clearByTabSessionId: clearByTabSessionId,' in cache_text
    assert 'cleanupStaleSessions: cleanupStaleSessions,' in cache_text
    assert 'StatePaths.UI_BUSY_GLOBAL' not in listener_runtime
    assert "ComponentBootstrapDependencyBuilder.build" in bootstrap_source
    assert "ComponentBootstrapDependencyBuilder.withManagerRuntime" in bootstrap_source
    assert 'getBackendMode: function () { return BackendModeContracts.MODES.REAL; }' in bootstrap_source
    assert 'BackendModeContracts.PATHS.BACKEND_MODE' in bootstrap_source
    assert "withManagerRuntime" in bootstrap_builder
    assert "MANAGER_KEYS" in bootstrap_builder
    assert "HeartbeatManager" in bootstrap_source
    assert "AutoSaveCoordinator" in bootstrap_source
    assert "Object.freeze({" in bootstrap_builder
    assert 'REAL: "real"' in backend_mode_contracts
    assert 'BACKEND_MODE: ModelPathContracts.BACKEND_MODE' in backend_mode_contracts
    for path in removed_framework_aliases:
        assert not path.exists(), f"alias-only framework file still present: {path}"


def test_component_runtime_uses_canonical_model_paths_and_backend_mode_contracts():
    root_id_runtime = _read(APP_ROOT / "service" / "framework" / "RootIdRuntime.js")
    telemetry_runtime = _read(APP_ROOT / "service" / "framework" / "TelemetryRuntime.js")
    autosave_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentAutosaveRuntime.js")
    cross_tab_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentCrossTabRuntime.js")
    polling_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentPollingRuntime.js")
    lock_events_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentLockEventsRuntime.js")
    open_detail_usecase = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "OpenDetailUseCase.js")
    shell_state_runtime = _read(APP_ROOT / "service" / "features" / "shell" / "runtime" / "ShellStateRuntime.js")
    diagnostics_usecase = _read(APP_ROOT / "service" / "domain" / "shared" / "usecases" / "InitializeAppUseCase.js")
    startup_capability_usecase = _read(APP_ROOT / "service" / "domain" / "shared" / "usecases" / "StartupCapabilityDiagnosticsUseCase.js")

    assert "ModelPathContracts.ACTIVE_OBJECT_ID" in root_id_runtime
    assert '"/activeObjectId"' not in telemetry_runtime
    assert "ModelPathContracts.ACTIVE_OBJECT_ID" in autosave_runtime
    assert "buildLatestCtx" in autosave_runtime
    assert "var oLatestCtx = fnBuildLatestCtx ? fnBuildLatestCtx() : oComponent._ctx;" in autosave_runtime
    assert "ModelPathContracts.ACTIVE_OBJECT_ID" in cross_tab_runtime
    assert "ModelPathContracts.ACTIVE_OBJECT_ID" in polling_runtime
    assert "ModelPathContracts.ACTIVE_OBJECT_ID" in lock_events_runtime
    assert "ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID" in open_detail_usecase
    assert "ModelPathContracts.SELECTED_ID" in open_detail_usecase
    assert "ModelPathContracts.BACKEND_MODE" in shell_state_runtime
    assert "BackendModeContracts.MODES.REAL" in diagnostics_usecase
    assert "BackendModeContracts.CAPABILITY.READY" in startup_capability_usecase


def test_route_runtime_is_manifest_first_without_route_sync_shadow_layer():
    route_mode = _read(APP_ROOT / "infra" / "navigation" / "RouteModeCoordinator.js")
    shell_layout_runtime = _read(APP_ROOT / "service" / "features" / "shell" / "runtime" / "ShellLayoutRuntime.js")
    state_paths = _read(APP_ROOT / "model" / "StatePaths.js")
    model_paths = _read(APP_ROOT / "service" / "domain" / "shared" / "ModelPathContracts.js")
    component_app_runtime = _read(APP_ROOT / "service" / "framework" / "ComponentAppRuntime.js")
    core_runtime_bootstrap = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentCoreRuntimeBootstrap.js")
    apply_runtime_settings = _read(APP_ROOT / "service" / "domain" / "shared" / "usecases" / "ApplyRuntimeSettingsUseCase.js")

    assert "RouteSync" not in route_mode
    assert "oRouteSync" not in route_mode
    assert "shellModel" not in route_mode
    assert "ModelPathContracts.LAYOUT" not in route_mode
    assert "resolveDesiredLayout" in shell_layout_runtime
    assert "MODEL_PATHS.SHELL_LAYOUT" in shell_layout_runtime
    assert "WORKFLOW_SEARCH_MODE" not in state_paths
    assert "WORKFLOW_SEARCH_SEGMENTS" not in state_paths
    assert "WORKFLOW_SEARCH_MODE" not in model_paths
    assert "WORKFLOW_SEARCH_SEGMENTS" not in model_paths
    assert "envState:" in component_app_runtime
    assert "envModel:" not in component_app_runtime
    assert "syncShellRuntimeState:" not in component_app_runtime
    assert "navigation:" not in core_runtime_bootstrap
    assert "ctx.envState" in apply_runtime_settings
    assert "ctx.envModel" not in apply_runtime_settings


def test_controller_model_access_is_explicit_without_generic_named_fallback_surface():
    model_access_mixin = _read(APP_ROOT / "controller" / "base" / "ModelAccessMixin.js")
    controller_model_runtime = _read(APP_ROOT / "service" / "framework" / "ControllerModelRuntime.js")

    assert "resolveNamedModel" in model_access_mixin
    assert "case MODELS.STATE:" in model_access_mixin
    assert "case MODELS.SHELL:" in model_access_mixin
    assert "case MODELS.DETAIL:" in model_access_mixin
    assert "case MODELS.MASTER_DATA:" in model_access_mixin
    assert "case MODELS.VIEW:" in model_access_mixin
    assert "return null;" in model_access_mixin
    assert "named:" not in controller_model_runtime
    assert "read:" not in controller_model_runtime
    assert "write:" not in controller_model_runtime
    assert "setMany:" not in controller_model_runtime


def test_lock_and_persistence_message_keys_are_centralized_in_constants():
    detail_persistence = _read(APP_ROOT / "service" / "domain" / "detail" / "DetailPersistenceRuntime.js")
    lock_adapter = _read(APP_ROOT / "infra" / "adapters" / "LockAdapter.js")
    component_lock_events = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentLockEventsRuntime.js")
    default_handlers = _read(APP_ROOT / "service" / "framework" / "behavior" / "WorkflowDefaultHandlers.js")
    detail_constants = _read(APP_ROOT / "constants" / "DetailUseCaseConstants.js")
    detail_message_keys = _read(APP_ROOT / "constants" / "DetailMessageKeyConstants.js")

    assert 'messageKey: "persistence' not in detail_persistence
    assert 'messageKey: "lock' not in lock_adapter
    assert 'Effects.warn("lockAcquireFailed")' not in APP_ROOT.joinpath("service", "domain", "detail", "usecases", "EnterEditUseCase.js").read_text(encoding="utf-8")
    assert '"lockLostMessage"' not in component_lock_events
    assert '"lockKilledMessage"' not in default_handlers
    assert "MESSAGE_KEYS:" not in detail_constants
    assert 'LOCK_LOST: "lockLostMessage"' in detail_message_keys
    assert 'PERSISTENCE_IDLE: "persistenceIdle"' in detail_message_keys


def test_backend_contract_service_is_typed_for_non_lock_responses():
    contract_service = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_contract_service.clas.abap")
    contract_constants = REPO_ROOT / "backend" / "sap_backend" / "src" / "zif_zodata_contract_constants.intf.abap"
    legacy_constants_class = REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_contract_constants.clas.abap"
    dpc_ext = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_dpc_ext.clas.abap")
    lock_manager = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_lock_manager.clas.abap")
    frontend_context_service = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_frontend_context_svc.clas.abap")
    mpl_service = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_mpl_service.clas.abap")
    lock_interface = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zif_zodata_lock_manager.intf.abap")

    assert "cs_result              TYPE zstr_pcct_lock_acquire_rs." in contract_service
    assert "cs_result              TYPE zstr_pcct_lock_heartbeat_rs." in contract_service
    assert "cs_result              TYPE zstr_pcct_lock_release_rs." in contract_service
    assert "cs_result         TYPE zstr_pcct_savechanges_rs." in contract_service
    assert "cs_result      TYPE zstr_pcct_permission_rs." in contract_service
    assert "cs_result           TYPE zstr_pcct_current_user_rs." in contract_service
    assert "cs_result      TYPE zstr_pcct_runtime_settings_rs." in contract_service
    assert "TYPE any" not in contract_service
    assert "TYPE any" not in lock_interface
    assert "TYPE any" not in lock_manager
    assert contract_constants.exists()
    assert not legacy_constants_class.exists()
    assert "zcl_zodata_contract_constants=>" not in contract_service
    assert "zcl_zodata_contract_constants=>" not in dpc_ext
    assert "zcl_zodata_contract_constants=>" not in lock_manager
    assert "zif_zodata_contract_constants=>" in contract_service
    assert "zif_zodata_contract_constants=>" in dpc_ext
    assert "zif_zodata_contract_constants=>" in lock_manager
    assert "build_permission_result" in frontend_context_service
    assert "build_current_user_result" in frontend_context_service
    assert "build_runtime_settings_result" in frontend_context_service
    assert "mo_frontend_context->build_permission_result" in dpc_ext
    assert "mo_frontend_context->build_current_user_result" in dpc_ext
    assert "mo_frontend_context->build_runtime_settings_result" in dpc_ext
    assert "mo_mpl_service->read_tree" in dpc_ext
    assert "CALL FUNCTION 'Z_PCCT_MPL_TREE_GET'" in mpl_service
    assert "CALL FUNCTION 'Z_PCCT_MPL_TREE_GET'" not in dpc_ext


def test_detail_search_and_shell_message_keys_live_only_in_dedicated_constant_modules():
    detail_formatters = _read(APP_ROOT / "service" / "features" / "detail" / "runtime" / "DetailFormatters.js")
    search_formatter = _read(APP_ROOT / "controller" / "search" / "SearchFormatterBehavior.js")
    search_dialogs = _read(APP_ROOT / "controller" / "search" / "SearchToolbarDialogFactoryRuntime.js")
    search_view_state = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewStateRuntime.js")
    shell_state = _read(APP_ROOT / "service" / "features" / "shell" / "runtime" / "ShellStateRuntime.js")
    ui_decision_defaults = _read(APP_ROOT / "service" / "framework" / "behavior" / "UiDecisionDefaultHandlers.js")
    search_toolbar_contracts = _read(APP_ROOT / "service" / "features" / "search" / "contracts" / "SearchToolbarContracts.js")

    assert '"statusRegistered"' not in detail_formatters
    assert '"requiredFieldHint"' not in detail_formatters
    assert '"autosaveWaiting"' not in detail_formatters
    assert '"searchModeLabel"' not in search_formatter
    assert '"resultsLabel"' not in search_formatter
    assert '"searchSortDialogTitle"' not in search_dialogs
    assert '"searchGroupDialogTitle"' not in search_dialogs
    assert '"workflowStageAnalyze"' not in search_view_state
    assert '"searchSortDateCheck"' not in search_toolbar_contracts
    assert '"searchGroupNone"' not in search_toolbar_contracts
    assert '"shellPermissionCreate"' not in shell_state
    assert '"shellContextDetail"' not in shell_state
    assert '"shellUserTooltipStandalone"' not in shell_state
    assert '"searchOpenUsesFirstHint"' not in ui_decision_defaults
    assert '"shellContextRefreshed"' not in ui_decision_defaults


def test_productive_runtime_has_no_raw_active_or_selected_id_paths_outside_canonical_contracts():
    allowed_suffixes = {
        str(APP_ROOT / "service" / "domain" / "shared" / "ModelPathContracts.js"),
        str(APP_ROOT / "service" / "runtime" / "component" / "ComponentListenerContracts.js"),
    }

    for path in APP_ROOT.rglob("*.js"):
        normalized = str(path)
        if "/app/test/" in normalized.replace("\\", "/"):
            continue
        if normalized in allowed_suffixes:
            continue
        text = _read(path)
        assert '"/activeObjectId"' not in text, f'raw activeObjectId path leaked: {path}'
        assert '"/selectedId"' not in text, f'raw selectedId path leaked: {path}'
        assert 'StatePaths.ACTIVE_OBJECT_ID || "/activeObjectId"' not in text, f'activeObjectId fallback leaked: {path}'
        assert 'ModelPathContracts.ACTIVE_OBJECT_ID || "/activeObjectId"' not in text, f'activeObjectId fallback leaked: {path}'


def test_open_detail_source_checks_permission_before_cache_and_backend_load():
    source = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "OpenDetailUseCase.js")

    non_create_branch = source[source.index("var oCacheValidation = mCtx && mCtx.cacheValidation;"):]
    permission_idx = non_create_branch.index("DetailAuthorizationRuntime.fetchPermission")
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
    source = _read(APP_ROOT / "infra" / "adapters" / "shared" / "ODataChecklistPermissionRuntime.js")
    key_contracts = _read(APP_ROOT / "infra" / "adapters" / "shared" / "ODataKeyContracts.js")
    readme = _read(BACKEND_ROOT / "README_ODATA.md")

    assert 'buildEntityPath("ChecklistCreatePermissionSet", "CURRENT"' in source
    assert 'CURRENT_ALIAS_KEY: "Edm.String"' in key_contracts
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
    assert "evergreen Microsoft Edge" in guide
    assert "Create a new checklist and complete the first save." in guide
    assert "Architectural baseline" in guide


def test_final_audit_docs_reflect_current_production_baseline():
    audit_md = _read(REPO_ROOT / "docs" / "artifacts" / "product-audit-current.md")
    audit_json = _read(REPO_ROOT / "docs" / "artifacts" / "product-audit-current.json")
    final_report_path = REPO_ROOT / "docs" / "artifacts" / "final-production-baseline.md"
    final_report = _read(final_report_path)

    assert final_report_path.exists()
    assert "production-grade baseline" in final_report
    assert "app/constants/*" in final_report
    assert "ModelPathContracts" in final_report
    assert "StatePaths" in final_report
    assert "plain modules/factories exposing `{ execute }`" in final_report
    assert "evergreen Microsoft Edge" in final_report
    assert "selectedId and activeObjectId are not consumed as uncontrolled parallel truths" not in audit_md
    assert '"status": "baseline_ready"' in audit_json
    assert "constants/contracts cleanup, `UseCase` migration, and CSS patch decoupling are complete" in audit_md


def test_explicit_delta_contract_is_canonical_for_save_and_autosave():
    delta_contracts = _read(APP_ROOT / "service" / "shared" / "delta" / "DeltaContracts.js")
    payload_builder = _read(APP_ROOT / "service" / "shared" / "DeltaPayloadBuilder.js")
    field_mappers = _read(APP_ROOT / "service" / "shared" / "delta" / "DeltaFieldMappers.js")
    payload_mapper = _read(APP_ROOT / "infra" / "adapters" / "shared" / "ODataChecklistPayloadMapper.js")
    save_usecase = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "SaveDetailUseCase.js")
    autosave_usecase = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "AutosaveDetailUseCase.js")
    delete_usecase = _read(APP_ROOT / "service" / "domain" / "detail" / "usecases" / "DeleteChecklistUseCase.js")
    search_startup = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchStartupRuntime.js")
    search_load_behavior = _read(APP_ROOT / "controller" / "search" / "internal" / "SearchViewLoadBehavior.js")
    search_rediscovery = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchReturnRediscoveryRuntime.js")
    state_paths = _read(APP_ROOT / "model" / "StatePaths.js")
    workflow_schema = _read(APP_ROOT / "model" / "schema" / "workflowSchema.js")
    abap_mapper = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_bopf_mapper.clas.abap")
    abap_dpc = _read(REPO_ROOT / "backend" / "sap_backend" / "src" / "zcl_zodata_dpc_ext.clas.abap")

    assert 'CREATE: "C"' in delta_contracts
    assert 'UPDATE: "U"' in delta_contracts
    assert 'DELETE: "D"' in delta_contracts
    assert 'PARTICIPANTS: "participants"' in delta_contracts
    assert 'ATTACHMENTS: "attachments"' in delta_contracts

    assert "participants:" in payload_builder
    assert "attachments:" in payload_builder
    assert "buildCreatePayload" in payload_builder
    assert "appendChildChanges" in payload_builder
    assert "formatODataDate" in payload_builder
    assert "rootEditMode" in payload_builder
    assert "toParticipantFields" in field_mappers
    assert "toAttachmentFields" in field_mappers
    assert "participants: Array.isArray(oIn.participants)" in payload_mapper
    assert "mergeDeltaAttachments" in save_usecase
    assert "mergeDeltaAttachments" in autosave_usecase
    assert 'ModelPathContracts.SEARCH_RETURN_CONTEXT' in save_usecase
    assert 'SearchReturnRediscoveryRuntime.MODES.CREATE' in save_usecase
    assert 'SearchReturnRediscoveryRuntime.MODES.SAVE' in save_usecase
    assert 'ChecklistIdentity.extractChecklistDisplayId' in save_usecase
    assert 'ModelPathContracts.SEARCH_FORCE_REFRESH_ON_RETURN' not in save_usecase
    assert 'ModelPathContracts.SEARCH_FORCE_REFRESH_ON_RETURN' not in autosave_usecase
    assert 'ModelPathContracts.SEARCH_RETURN_CONTEXT' not in autosave_usecase
    assert 'ModelPathContracts.SEARCH_RETURN_CONTEXT' in delete_usecase
    assert 'SearchReturnRediscoveryRuntime.MODES.DELETE' in delete_usecase
    assert 'SEARCH_RETURN_CONTEXT: "/searchReturnContext"' in state_paths
    assert "searchReturnContext: null," in workflow_schema
    assert "SearchReturnRediscoveryRuntime.readContext" in search_startup
    assert "SearchReturnRediscoveryRuntime.hasLegacyRefreshFlag" in search_startup
    assert "SearchReturnRediscoveryRuntime.applyAfterSearchSuccess" in search_load_behavior
    assert "selectionRequested" in search_rediscovery
    assert "focusRequested" in search_rediscovery
    assert "ChecklistIdentity.extractChecklistId" in search_rediscovery
    assert "ChecklistIdentity.extractChecklistDisplayId" in search_rediscovery
    assert "is_root-edit_mode IS NOT INITIAL" in abap_mapper
    assert "METHOD validate_save_request." in abap_dpc
    assert "checks[].edit_mode is required." in abap_dpc
    assert "barriers[].edit_mode is required." in abap_dpc
    assert "participants[].edit_mode is required." in abap_dpc
    assert "attachments[].edit_mode is required." in abap_dpc


def test_active_frontend_mutations_use_hybrid_aggregate_function_import_contract():
    mutation_runtime = _read(APP_ROOT / "infra" / "adapters" / "shared" / "ODataChecklistMutationRuntime.js")
    gateway_client = _read(APP_ROOT / "service" / "backend" / "GatewayClient.js")
    gateway_odata_client = _read(APP_ROOT / "infra" / "odata" / "GatewayODataClient.js")
    gateway_contracts = _read(APP_ROOT / "service" / "backend" / "GatewayClientContracts.js")
    canonical_api = _read(BACKEND_ROOT / "api" / "gateway_canonical_api.py")

    assert "FUNCTION_IMPORTS.SAVE_CHANGES" in mutation_runtime
    assert "FUNCTION_IMPORTS.AUTO_SAVE" in mutation_runtime
    assert "FUNCTION_IMPORTS.CREATE_CHECKLIST" in mutation_runtime
    assert "FUNCTION_IMPORTS.COPY_CHECKLIST" in mutation_runtime
    assert "normalizeSavePayload" in mutation_runtime
    assert "GatewayODataClient.post(" not in mutation_runtime
    assert "GatewayODataClient.patch(" not in mutation_runtime
    assert "createPath: function" not in gateway_client
    assert "updatePath: function" not in gateway_client
    assert "patch: patch," not in gateway_odata_client
    assert "post: post," not in gateway_odata_client
    assert 'entityDeletePattern(GatewayContractConstants.ENTITY_SETS.CHECKLIST_CHECK' in gateway_contracts
    assert 'entityDeletePattern(GatewayContractConstants.ENTITY_SETS.CHECKLIST_BARRIER' in gateway_contracts
    assert '@router.post(f"{SERVICE_ROOT}/SaveChanges")' in canonical_api
    assert '@router.post(f"{SERVICE_ROOT}/AutoSave")' in canonical_api
    assert '@router.post(f"{SERVICE_ROOT}/CreateChecklist")' in canonical_api
    assert '@router.post(f"{SERVICE_ROOT}/CopyChecklist")' in canonical_api


def test_start_local_env_supports_python_fallback_and_external_gateway_mode():
    source = _read(REPO_ROOT / "scripts" / "start-local-env.ps1")

    assert "PYTHON_BIN" in source
    assert "py -3 launcher" in source
    assert "python on PATH" in source
    assert "FastAPI and uvicorn" in source
    assert "-GatewayBaseUrl" in source or "GatewayBaseUrl" in source


def test_telemetry_uses_canonical_lock_state_vocabulary():
    telemetry_runtime = _read(APP_ROOT / "service" / "framework" / "TelemetryRuntime.js")
    workflow_telemetry = _read(APP_ROOT / "service" / "framework" / "WorkflowTelemetry.js")
    lock_gate = _read(REPO_ROOT / "scripts" / "gates" / "lock-state-enum-gate.js")

    assert "lockState:" in telemetry_runtime
    assert "lockOperationState:" not in telemetry_runtime
    assert "lockState:" in workflow_telemetry
    assert "lockOperationState:" not in workflow_telemetry
    assert "lockState" in lock_gate


def test_search_request_window_supports_legacy_search_max_results_shape():
    source = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewStateRuntime.js")
    util_source = _read(APP_ROOT / "service" / "features" / "search" / "contracts" / "SearchMaxResults.js")

    assert 'typeof SearchMaxResults.resolveGrowingPageSize === "function"' in source
    assert 'typeof SearchMaxResults.resolveMaxResults === "function"' in source
    assert 'resolveGrowingPageSize: resolveGrowingPageSize,' in util_source


def test_sticky_runtime_is_route_scoped_without_global_body_observer():
    legacy_path = APP_ROOT / "search-toolbar-sticky-runtime.js"
    runtime_source = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewportRuntime.js")
    binding_source = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchViewportBindingRuntime.js")

    assert not legacy_path.exists()
    assert "MutationObserver" not in runtime_source
    assert "MutationObserver" not in binding_source
    assert 'new window.ResizeObserver(function () {' in binding_source
    assert 'window.addEventListener("resize", oController._fnSearchViewportResize);' in binding_source
    assert 'window.removeEventListener("resize", oController._fnSearchViewportResize);' in binding_source
    assert 'oScrollHost.addEventListener("scroll", oController._fnSearchScrollSync, { passive: true });' in binding_source
    assert 'oObserver.disconnect();' in binding_source


def test_detail_meta_bucket_is_explicit_and_synced_from_canonical_state():
    state_paths = _read(APP_ROOT / "model" / "StatePaths.js")
    workflow_schema = _read(APP_ROOT / "model" / "schema" / "workflowSchema.js")
    listener_runtime = _read(APP_ROOT / "service" / "runtime" / "component" / "ComponentDetailMetaSyncRuntime.js")

    assert 'DETAIL_META: "/detailMeta",' in state_paths
    assert 'detailMeta:' in workflow_schema
    assert 'ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {' in listener_runtime


def test_standardized_telemetry_event_names_cover_permission_cache_lock_and_analytics_flows():
    permission_source = _read(APP_ROOT / "service" / "domain" / "detail" / "DetailAuthorizationRuntime.js")
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


def test_detail_save_runtime_preserves_partial_basic_snapshots_after_backend_save():
    source = _read(APP_ROOT / "service" / "domain" / "detail" / "DetailSaveRuntime.js")

    assert "var mMergedFields = Object.assign({}, oBaseBasic, oCurrentBasic);" in source
    assert "Object.keys(mMergedFields).forEach(function (sField) {" in source
    assert "if (isFilled(oCurrentBasic[sField])) {" in source


def test_patch_css_prefers_semantic_host_classes_over_broad_renderer_selectors():
    patch_css = _read(APP_ROOT / "styles" / "modules" / "90_ui5_patches.css")
    layout_patch_css = _read(APP_ROOT / "styles" / "modules" / "91_ui5_layout_patches.css")
    search_sticky_runtime = _read(APP_ROOT / "service" / "features" / "search" / "runtime" / "SearchStickyLayoutRuntime.js")
    shell_dom_runtime = _read(APP_ROOT / "service" / "features" / "shell" / "runtime" / "AppShellDomRuntime.js")

    assert ".appFeedbackCorrelationInput .sapMInputBaseContentWrapper" in patch_css
    assert ".appFeedbackCorrelation .sapMInputBaseContentWrapper" not in patch_css
    assert ".appShellHeader .sapMBtnContent" not in patch_css
    assert ".shellActionBtn .sapMBtnContent" in patch_css
    assert ".pageTransparent .sapMScrollCont" in layout_patch_css
    assert ".chkApp .sapMScrollCont," not in layout_patch_css
    assert ".appRootFclTransparent .sapMNav" in layout_patch_css
    assert ".appRootFcl .sapMNav" not in layout_patch_css
    assert ".searchPage.sapMPage > .sapUiXMLView" in layout_patch_css
    assert ".detailGridTable .sapUiTableCtrlScr" in layout_patch_css
    assert ".flatEditorTable .sapMListTblCnt" in layout_patch_css
    assert "AppShellDomRuntime.resolveShellHeaderHostDom" in search_sticky_runtime
    assert "function resolveShellHeaderHostDom" in shell_dom_runtime


def test_expanded_rows_dialog_is_shared_across_checks_and_barriers_and_smoke_ids_are_current():
    detail_behavior = _read(APP_ROOT / "controller" / "detail" / "DetailChecklistBehavior.js")
    effect_dialog_runtime = _read(APP_ROOT / "service" / "framework" / "execution" / "EffectDialogRuntime.js")
    lazy_dialog_runtime = _read(APP_ROOT / "service" / "framework" / "LazyDialogRuntime.js")
    interaction_smoke = _read(REPO_ROOT / "scripts" / "interaction-smoke.py")
    manual_p1p2 = _read(REPO_ROOT / "scripts" / "manual-p1p2-browser-pass.py")

    assert 'dialogId: bExpandedRowsDialog ? "expandedRowsDialog" : undefined' in detail_behavior
    assert 'DialogContracts.IDS.CHECKS_EXPANDED' in effect_dialog_runtime
    assert 'DialogContracts.IDS.BARRIERS_EXPANDED' in effect_dialog_runtime
    assert 'dialogId: "expandedRowsDialog"' in effect_dialog_runtime
    assert 'if (mCache[sDialogId]) {' in lazy_dialog_runtime
    assert 'mCache[sDialogId] = oDialog;' in lazy_dialog_runtime
    assert 'DETAIL_VIEW_ID = "checklist_app_comp---detailView"' in interaction_smoke
    assert 'SEARCH_VIEW_ID = "checklist_app_comp---searchView"' in interaction_smoke
    assert 'dialogSelector": "[id$=\'expandedRowsDialog\']"' in interaction_smoke
    assert 'DETAIL_VIEW_ID = "checklist_app_comp---detailView"' in manual_p1p2
    assert 'SEARCH_VIEW_ID = "checklist_app_comp---searchView"' in manual_p1p2


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
