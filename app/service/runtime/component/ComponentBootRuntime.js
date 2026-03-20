sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootStageRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootStageExecutionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ComponentBootStageRuntime, ComponentBootStageExecutionRuntime, ComponentBootStateRuntime, ComponentBootContracts, ModelStateRuntime) {
    "use strict";

    var STAGE_ERRORS = ComponentBootContracts.STAGE_ERRORS;
    var READINESS_STATUS = ComponentBootContracts.READINESS_STATUS;
    var READINESS_APP_PATH = "/readiness/app";

    function cleanupCacheSessions(oCacheAdapter, oStateModel, sTabSessionId) {
        return Promise.resolve(
            oCacheAdapter && typeof oCacheAdapter.cleanupStaleSessions === "function"
                ? oCacheAdapter.cleanupStaleSessions(sTabSessionId)
                : null
        ).then(function () {
            return ComponentBootStageExecutionRuntime.cleanupSessions(oCacheAdapter, oStateModel, sTabSessionId);
        });
    }

    function validateStageResults(aStageResults) {
        var oBootError = ComponentBootStageRuntime.resolveSettledStageError(aStageResults[0], "load_current_user_failed") ||
            ComponentBootStageRuntime.resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed") ||
            ComponentBootStageRuntime.resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed");
        if (oBootError) {
            throw oBootError;
        }
        return aStageResults;
    }

    function runBootSequence(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oEnvModel = mOptions.envModel;
        var oCacheModel = mOptions.cacheModel;
        var InitializeAppUseCase = mOptions.initializeAppUseCase;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var bBootCompleted = false;
        var sTabSessionId = "";

        ComponentBootStateRuntime.initializeBootState(oStateModel);

        return Promise.resolve(InitializeAppUseCase.execute({}, { stateModel: oStateModel })).then(function (oBootstrapResult) {
            if (oBootstrapResult && oBootstrapResult.ok === false) {
                throw ComponentBootStageRuntime.toStageError(oBootstrapResult.error && oBootstrapResult.error.message, STAGE_ERRORS.BOOTSTRAP_APP_FAILED);
            }
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            sTabSessionId = ComponentRuntimeSupport.ensureTabSessionId(oStateModel);
            ComponentBootStateRuntime.seedFrontendState(oStateModel, oEnvModel);
            return cleanupCacheSessions(mOptions.cacheAdapter, oStateModel, sTabSessionId).then(function () {
                return ComponentBootStageExecutionRuntime.runBootStages({
                    component: oComponent,
                    ensureDictLoadedUseCase: mOptions.ensureDictLoadedUseCase,
                    loadCurrentUser: mOptions.loadCurrentUser,
                    loadRuntimeSettings: mOptions.loadRuntimeSettings
                });
            }).then(validateStageResults).then(function () {
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                var sReadyAt = new Date().toISOString();
                ComponentBootStateRuntime.finalizeBootSuccess({
                    stateModel: oStateModel,
                    cacheModel: oCacheModel,
                    cacheAt: sCacheAt,
                    readyAt: sReadyAt,
                    tabSessionId: sTabSessionId,
                    serverState: null,
                    checkLists: []
                });
                ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {
                    status: READINESS_STATUS.READY,
                    ready: true,
                    readyAt: sReadyAt,
                    error: ""
                });
                bBootCompleted = true;
            });
        }).catch(function (oError) {
            var sErrorMessage = String((oError && oError.message) || oError || STAGE_ERRORS.BOOT_FAILED);
            ComponentBootStateRuntime.finalizeBootError(oStateModel, sErrorMessage, mOptions.bundleText, sTabSessionId);
            return null;
        }).finally(function () {
            if (bBootCompleted) {
                oComponent._startCoreManagers();
                oComponent._syncLockScopedManagers(oStateModel);
            }
            // Keep the final loading flip explicit in the facade for boot-success seam invariants.
            mOptions.componentRuntimeSupport && mOptions.componentRuntimeSupport.syncUiStateMode && mOptions.componentRuntimeSupport.syncUiStateMode(oStateModel, mOptions.uiStateModel || null);
            oStateModel.setProperty("/isLoading", false);
        });
    }

    return {
        runBootSequence: runBootSequence
    };
});
