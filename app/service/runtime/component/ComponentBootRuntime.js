sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts"
], function (ComponentBootContracts, ModelStateRuntime, CloneUtil, WorkflowTelemetry, FrontendConfigConstants, EffectFeedbackContracts) {
    "use strict";

    var PATHS = ComponentBootContracts.PATHS;
    var STAGE_ERRORS = ComponentBootContracts.STAGE_ERRORS;
    var READINESS_STATUS = ComponentBootContracts.READINESS_STATUS;
    var CONFIG_SOURCE = ComponentBootContracts.FRONTEND_CONFIG_SOURCE;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;

    function initializeBootState(oStateModel) {
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": false,
            "/locationsLoading": false,
            "/readiness/app": {
                status: READINESS_STATUS.LOADING,
                ready: false,
                readyAt: "",
                error: ""
            }
        });
    }

    function seedFrontendState(oStateModel, oEnvModel) {
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.CURRENT_USER, {
            fullName: "",
            permissions: [],
            permissionRules: [],
            canView: false,
            canEdit: false,
            canDelete: false,
            summaryText: "",
            fetchedAt: ""
        });
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/requiredFields": [],
            "/frontendVariables": Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES),
            "/frontendConfigSource": CONFIG_SOURCE.GATEWAY
        });
        ModelStateRuntime.writeOnModel(oEnvModel, "/variables", Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES));
    }

    function finalizeBootSuccess(mOptions) {
        var oStateModel = mOptions.stateModel;
        var oCacheModel = mOptions.cacheModel;
        var sCacheAt = mOptions.cacheAt;
        var sReadyAt = mOptions.readyAt;
        var sTabSessionId = mOptions.tabSessionId;
        var oServerState = mOptions.serverState;
        var aCheckLists = mOptions.checkLists || [];

        ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
        ModelStateRuntime.setManyOnModel(oCacheModel, {
            "/lastServerState": oServerState || {
                fetchedAt: sCacheAt,
                count: aCheckLists.length
            },
            "/keyMapping": {}
        });
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.CACHE_VALIDATION_AT, sCacheAt);
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.READINESS_APP, {
            status: READINESS_STATUS.READY,
            ready: true,
            readyAt: sReadyAt,
            error: ""
        });
        WorkflowTelemetry.emit("boot.readiness.ready", {
            stateModel: oStateModel,
            payload: {
                readyAt: sReadyAt,
                activeTabSessionId: sTabSessionId
            }
        });
    }

    function finalizeBootError(oStateModel, sErrorMessage, fnBundleText, sTabSessionId) {
        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/loadError": true,
            "/loadErrorMessage": fnBundleText(FALLBACK_TEXT_KEYS.LOAD_ERROR) + ": " + sErrorMessage
        });
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.READINESS_APP, {
            status: READINESS_STATUS.ERROR,
            ready: false,
            readyAt: "",
            error: sErrorMessage
        });
        WorkflowTelemetry.emit("boot.readiness.error", {
            stateModel: oStateModel,
            payload: {
                error: sErrorMessage,
                activeTabSessionId: sTabSessionId
            }
        });
    }

    function allSettledPolyfill(aPromises) {
        return Promise.all((aPromises || []).map(function (p) {
            return Promise.resolve(p).then(
                function (v) { return { status: "fulfilled", value: v }; },
                function (e) { return { status: "rejected", reason: e }; }
            );
        }));
    }

    function toStageError(vError, sFallbackMessage) {
        if (vError instanceof Error) {
            return vError;
        }
        return new Error(String(vError || sFallbackMessage || "boot_stage_failed"));
    }

    function resolveSettledStageError(oSettledResult, sFallbackMessage) {
        if (!oSettledResult) {
            return toStageError(null, sFallbackMessage);
        }
        if (oSettledResult.status === "rejected") {
            return toStageError(oSettledResult.reason, sFallbackMessage);
        }
        if (oSettledResult.value && oSettledResult.value.ok === false) {
            return toStageError(oSettledResult.value.error && oSettledResult.value.error.message, sFallbackMessage);
        }
        return null;
    }

    function cleanupCacheSessions(oCacheAdapter, oStateModel, sTabSessionId) {
        return Promise.resolve(
            oCacheAdapter && typeof oCacheAdapter.cleanupStaleSessions === "function"
                ? oCacheAdapter.cleanupStaleSessions(sTabSessionId)
                : null
        ).then(function (oCleanupResult) {
            if (oCleanupResult) {
                WorkflowTelemetry.emit("cache.session.cleanup", {
                    stateModel: oStateModel,
                    payload: {
                        activeTabSessionId: sTabSessionId,
                        cleared: Number(oCleanupResult.cleared || 0) || 0
                    }
                });
            }
            return oCleanupResult;
        });
    }

    function runBootStages(mOptions) {
        return allSettledPolyfill([
            Promise.resolve(typeof mOptions.loadCurrentUser === "function" ? mOptions.loadCurrentUser() : null),
            Promise.resolve(typeof mOptions.loadRuntimeSettings === "function" ? mOptions.loadRuntimeSettings() : null),
            Promise.resolve(mOptions.ensureDictLoadedUseCase.execute({}, mOptions.component._ctx))
        ]).then(function (aStageResults) {
            var oBootError = resolveSettledStageError(aStageResults[0], STAGE_ERRORS.LOAD_CURRENT_USER_FAILED) ||
                resolveSettledStageError(aStageResults[1], STAGE_ERRORS.LOAD_RUNTIME_SETTINGS_FAILED) ||
                resolveSettledStageError(aStageResults[2], STAGE_ERRORS.BOOTSTRAP_INIT_BUNDLE_FAILED);
            if (oBootError) {
                throw oBootError;
            }
            return aStageResults;
        });
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

        initializeBootState(oStateModel);

        return Promise.resolve(InitializeAppUseCase.execute({}, { stateModel: oStateModel }))
            .then(function (oBootstrapResult) {
                if (oBootstrapResult && oBootstrapResult.ok === false) {
                    throw toStageError(oBootstrapResult.error && oBootstrapResult.error.message, STAGE_ERRORS.BOOTSTRAP_APP_FAILED);
                }
                ComponentRuntimeSupport.ensureSessionId(oStateModel);
                sTabSessionId = ComponentRuntimeSupport.ensureTabSessionId(oStateModel);
                seedFrontendState(oStateModel, oEnvModel);
                return cleanupCacheSessions(mOptions.cacheAdapter, oStateModel, sTabSessionId);
            })
            .then(function () {
                return runBootStages({
                    component: oComponent,
                    ensureDictLoadedUseCase: mOptions.ensureDictLoadedUseCase,
                    loadCurrentUser: mOptions.loadCurrentUser,
                    loadRuntimeSettings: mOptions.loadRuntimeSettings
                });
            })
            .then(function () {
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                var sReadyAt = new Date().toISOString();

                finalizeBootSuccess({
                    stateModel: oStateModel,
                    cacheModel: oCacheModel,
                    cacheAt: sCacheAt,
                    readyAt: sReadyAt,
                    tabSessionId: sTabSessionId,
                    serverState: null,
                    checkLists: []
                });
                bBootCompleted = true;
            })
            .catch(function (oError) {
                var sErrorMessage = String((oError && oError.message) || oError || STAGE_ERRORS.BOOT_FAILED);
                finalizeBootError(oStateModel, sErrorMessage, mOptions.bundleText, sTabSessionId);
                return null;
            })
            .finally(function () {
                if (bBootCompleted) {
                    oComponent._startCoreManagers();
                    oComponent._syncLockScopedManagers(oStateModel);
                }
                mOptions.componentRuntimeSupport && mOptions.componentRuntimeSupport.syncUiStateMode && mOptions.componentRuntimeSupport.syncUiStateMode(oStateModel, mOptions.uiStateModel || null);
                oStateModel.setProperty("/isLoading", false);
            });
    }

    return {
        allSettledPolyfill: allSettledPolyfill,
        cleanupCacheSessions: cleanupCacheSessions,
        finalizeBootError: finalizeBootError,
        finalizeBootSuccess: finalizeBootSuccess,
        initializeBootState: initializeBootState,
        runBootSequence: runBootSequence,
        runBootStages: runBootStages,
        seedFrontendState: seedFrontendState,
        toStageError: toStageError
    };
});
