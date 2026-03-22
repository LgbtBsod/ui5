sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (ComponentBootstrapContracts, ModelStateRuntime, CloneUtil, WorkflowTelemetry, FrontendConfigConstants, EffectFeedbackContracts, ShellStateRuntime) {
    "use strict";

    var PATHS = ComponentBootstrapContracts.PATHS;
    var STAGE_ERRORS = ComponentBootstrapContracts.STAGE_ERRORS;
    var READINESS_STATUS = ComponentBootstrapContracts.READINESS_STATUS;
    var CONFIG_SOURCE = ComponentBootstrapContracts.FRONTEND_CONFIG_SOURCE;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;

    function hasServerDataSnapshot(aCheckLists, oServerState) {
        return !!((Array.isArray(aCheckLists) && aCheckLists.length > 0) || oServerState);
    }

    function initializeBootState(oStateModel) {
        ModelStateRuntime.setManyOnModel(oStateModel, {
            [PATHS.IS_LOADING]: true,
            [PATHS.MASTER_DATA_LOADING]: false,
            [PATHS.LOCATIONS_LOADING]: false,
            [PATHS.READINESS_APP]: {
                status: READINESS_STATUS.LOADING,
                ready: false,
                readyAt: "",
                error: ""
            }
        });
    }

    function seedFrontendState(oStateModel, oEnvState) {
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
        if (oEnvState) {
            oEnvState.variables = Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES);
        }
    }

    function finalizeBootSuccess(mOptions) {
        var oStateModel = mOptions.stateModel;
        var oCacheState = mOptions.cacheState;
        var sCacheAt = mOptions.cacheAt;
        var sReadyAt = mOptions.readyAt;
        var sTabSessionId = mOptions.tabSessionId;
        var oServerState = mOptions.serverState;
        var aCheckLists = mOptions.checkLists || [];

        if (oCacheState && hasServerDataSnapshot(aCheckLists, oServerState)) {
            oCacheState.pristineSnapshot = CloneUtil.clone(aCheckLists, []);
            oCacheState.lastServerState = oServerState || {
                fetchedAt: sCacheAt,
                count: aCheckLists.length
            };
            oCacheState.keyMapping = {};
        }
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
        var oEnvState = mOptions.envState;
        var oCacheState = mOptions.cacheState;
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
                seedFrontendState(oStateModel, oEnvState);
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
                    cacheState: oCacheState,
                    cacheAt: sCacheAt,
                    readyAt: sReadyAt,
                    tabSessionId: sTabSessionId,
                    serverState: oCacheState && oCacheState.lastServerState ? oCacheState.lastServerState : null,
                    checkLists: oCacheState && Array.isArray(oCacheState.pristineSnapshot) ? oCacheState.pristineSnapshot : null
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
                ShellStateRuntime.syncRuntimeShellState(oStateModel, mOptions.shellModel || null);
                oStateModel.setProperty(PATHS.IS_LOADING, false);
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
