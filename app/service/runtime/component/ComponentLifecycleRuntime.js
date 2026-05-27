sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (
    ComponentBootstrapContracts,
    BehaviorScopes,
    ModelStateRuntime,
    CloneUtil,
    WorkflowTelemetry,
    FrontendConfigConstants,
    EffectFeedbackContracts,
    ShellStateRuntime
) {
    "use strict";

    var PATHS = ComponentBootstrapContracts.PATHS;
    var STAGE_ERRORS = ComponentBootstrapContracts.STAGE_ERRORS;
    var READINESS_STATUS = ComponentBootstrapContracts.READINESS_STATUS;
    var CONFIG_SOURCE = ComponentBootstrapContracts.FRONTEND_CONFIG_SOURCE;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;

    function buildTelemetryManagerOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            detailModel: mModels.detailModel,
            timerDefaults: mTelemetry.timerDefaults,
            managers: mDeps.managers,
            statePaths: mDeps.StatePaths,
            deltaPayloadBuilder: mDeps.DeltaPayloadBuilder,
            buildLatestCtx: mHandlers.buildLatestCtx,
            resolveDetailCurrent: mHandlers.resolveDetailCurrent,
            applyFacadeResult: mHandlers.applyFacadeResult,
            setGlobalBanner: mHandlers.setGlobalBanner,
            emitTelemetry: mTelemetry.emitTelemetry,
            debugLogger: mDeps.DebugLogger,
            actionContract: mDeps.ActionContract,
            bundleText: mTelemetry.bundleText,
            componentRuntimeSupport: oRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime
        };
    }

    function buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            mainServiceModel: mModels.mainServiceModel,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            cacheState: mModels.cacheState,
            statePaths: mDeps.StatePaths,
            componentRuntimeSupport: oRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            debugLogger: mDeps.DebugLogger,
            bundleText: mTelemetry.bundleText,
            emitTelemetry: mTelemetry.emitTelemetry,
            setGlobalBanner: mHandlers.setGlobalBanner,
            handleForceReadOnly: mHandlers.handleForceReadOnly,
            applyFacadeResult: mHandlers.applyFacadeResult,
            telemetryRuntime: mDeps.TelemetryRuntime
        };
    }

    function buildInitListenerOptions(oComponent, mDeps, mModels, mHandlers, mServices, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            detailModel: mModels.detailModel,
            masterDataModel: mModels.masterDataModel,
            statePaths: mDeps.StatePaths,
            searchConfig: mServices.searchConfig,
            componentRuntimeSupport: oRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            bundleText: mTelemetry.bundleText,
            setGlobalBanner: mHandlers.setGlobalBanner,
            clearGlobalBanner: mHandlers.clearGlobalBanner,
            handleForceReadOnly: mHandlers.handleForceReadOnly,
            runGuardedSave: mHandlers.runGuardedSave,
            queuePendingNavigationIntent: mHandlers.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mHandlers.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mHandlers.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mHandlers.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mHandlers.restorePendingNavigationIntent,
            emitTelemetry: mTelemetry.emitTelemetry,
            publishTabSignal: mHandlers.publishTabSignal,
            telemetryRuntime: mDeps.TelemetryRuntime,
            layoutStateRuntime: mDeps.LayoutStateRuntime,
            actionContract: mDeps.ActionContract
        };
    }

    function initializeRouter(oComponent) {
        var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
        if (!oRouter || typeof oRouter.initialize !== "function" || oComponent._routerInitialized) {
            return;
        }
        oRouter.initialize();
        oComponent._routerInitialized = true;
    }

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

    function navigationScope() {
        return BehaviorScopes.navigation;
    }

    function runNavigationOperation(sOperation, mContext) {
        return navigationScope().executeSync(sOperation, mContext || {});
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
            Promise.resolve(typeof mOptions.initializeApp === "function" ? mOptions.initializeApp() : null),
            Promise.resolve(typeof mOptions.loadCurrentUser === "function" ? mOptions.loadCurrentUser() : null),
            Promise.resolve(typeof mOptions.loadRuntimeSettings === "function" ? mOptions.loadRuntimeSettings() : null),
            Promise.resolve(mOptions.ensureDictLoadedUseCase.execute({}, mOptions.component._ctx))
        ]).then(function (aStageResults) {
            var oBootError = resolveSettledStageError(aStageResults[0], STAGE_ERRORS.BOOTSTRAP_APP_FAILED) ||
                resolveSettledStageError(aStageResults[1], STAGE_ERRORS.LOAD_CURRENT_USER_FAILED) ||
                resolveSettledStageError(aStageResults[2], STAGE_ERRORS.LOAD_RUNTIME_SETTINGS_FAILED) ||
                resolveSettledStageError(aStageResults[3], STAGE_ERRORS.BOOTSTRAP_INIT_BUNDLE_FAILED);
            if (oBootError) {
                throw oBootError;
            }
            return aStageResults;
        });
    }


    function runBootSetup(mOptions) {
        var oStateModel = mOptions.stateModel;
        seedFrontendState(oStateModel, mOptions.envState);
        mOptions.componentRuntimeSupport.ensureSessionId(oStateModel);
        return Promise.resolve(mOptions.componentRuntimeSupport.ensureTabSessionId(oStateModel)).then(function (sTabSessionId) {
            return cleanupCacheSessions(mOptions.cacheAdapter, oStateModel, sTabSessionId).then(function () {
                return sTabSessionId;
            });
        });
    }

    function createInitializeAppRunner(mOptions) {
        return function () {
            return Promise.resolve(
                typeof mOptions.initializeApp === "function"
                    ? mOptions.initializeApp()
                    : mOptions.initializeAppUseCase.execute({}, { stateModel: mOptions.stateModel })
            ).then(function (oBootstrapResult) {
                if (oBootstrapResult && oBootstrapResult.ok === false) {
                    throw toStageError(oBootstrapResult.error && oBootstrapResult.error.message, STAGE_ERRORS.BOOTSTRAP_APP_FAILED);
                }
                return oBootstrapResult;
            });
        };
    }

    function runBootManagers(oComponent, oStateModel, bBootCompleted, oShellModel) {
        if (bBootCompleted) {
            oComponent._startCoreManagers();
            oComponent._syncLockScopedManagers(oStateModel);
        }
        ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel || null);
        oStateModel.setProperty(PATHS.IS_LOADING, false);
    }

    function runBootSequence(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oCacheState = mOptions.cacheState;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var bBootCompleted = false;
        var sTabSessionId = "";

        initializeBootState(oStateModel);

        return runBootSetup(mOptions).then(function (sResolvedTabSessionId) {
            sTabSessionId = sResolvedTabSessionId || "";
            return runBootStages({
                component: oComponent,
                initializeApp: createInitializeAppRunner(mOptions),
                ensureDictLoadedUseCase: mOptions.ensureDictLoadedUseCase,
                loadCurrentUser: mOptions.loadCurrentUser,
                loadRuntimeSettings: mOptions.loadRuntimeSettings
            });
        }).then(function () {
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
        }).catch(function (oError) {
            var sErrorMessage = String((oError && oError.message) || oError || STAGE_ERRORS.BOOT_FAILED);
            finalizeBootError(oStateModel, sErrorMessage, mOptions.bundleText, sTabSessionId);
            return null;
        }).finally(function () {
            runBootManagers(oComponent, oStateModel, bBootCompleted, mOptions.shellModel);
        });
    }


    function resolveRuntimeServices(oRuntimeContext) {
        return oRuntimeContext.services || {
            componentRuntimeSupport: oRuntimeContext.ComponentRuntimeSupport || oRuntimeContext.componentRuntimeSupport,
            searchConfig: oRuntimeContext.SearchUiConfig || oRuntimeContext.searchConfig
        };
    }

    function resolveRuntimeTelemetry(oRuntimeContext, mModels) {
        return oRuntimeContext.telemetry || {
            timerDefaults: ModelStateRuntime.readOnModel(mModels.stateModel, "/timers", {}) || {},
            emitTelemetry: oRuntimeContext.emitTelemetry || function () { return null; },
            bundleText: oRuntimeContext.bundleText || function (sKey) { return String(sKey || ""); }
        };
    }

    function resolveRuntimeHandlers(oComponent, mServices, oRuntimeContext) {
        return oRuntimeContext.handlers || {
            buildLatestCtx: function () { return oComponent._ctx; },
            resolveDetailCurrent: function () {
                return mServices.componentRuntimeSupport && typeof mServices.componentRuntimeSupport.resolveDetailCurrent === "function"
                    ? mServices.componentRuntimeSupport.resolveDetailCurrent(oComponent)
                    : {};
            },
            applyFacadeResult: function () { return null; },
            setGlobalBanner: function () { return null; },
            clearGlobalBanner: function () { return null; },
            handleForceReadOnly: function () { return null; },
            runGuardedSave: function () { return Promise.resolve(false); },
            queuePendingNavigationIntent: function () { return null; },
            clearPendingNavigationIntent: function () { return null; },
            revertPendingNavigationIntent: function () { return Promise.resolve(false); },
            resumePendingNavigationIntent: function () { return Promise.resolve(false); },
            restorePendingNavigationIntent: function () { return Promise.resolve(false); },
            publishTabSignal: function () { return null; }
        };
    }


    function attachRuntimeManagers(oComponent, mDeps, mModels, mHandlers, mTelemetry, mServices, oRuntimeSupport) {
        var oTelemetryOptions = buildTelemetryManagerOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport);
        mDeps.ComponentPollingRuntime.createHeartbeatManager(oTelemetryOptions);
        mDeps.ComponentPollingRuntime.createSupportManagers({
            component: oComponent,
            timerDefaults: mTelemetry.timerDefaults,
            managers: mDeps.managers
        });
        mDeps.ComponentAutosaveRuntime.createAutoSaveManager(oTelemetryOptions);
        mDeps.ComponentPollingRuntime.createLockStatusManager(oTelemetryOptions);
        mDeps.ComponentLockEventsRuntime.attachLockRuntime(
            buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport)
        );
        mDeps.ComponentInitListenersRuntime.attachInitListeners(
            buildInitListenerOptions(oComponent, mDeps, mModels, mHandlers, mServices, mTelemetry, oRuntimeSupport)
        );
    }

    function buildNavigationIntentApi(oComponent, mDeps, oStateModel) {
        return {
            queuePendingNavigationIntent: function (oRouteEvent, mIntentOptions) {
                runNavigationOperation("queuePendingIntent", {
                    component: oComponent,
                    stateModel: oStateModel,
                    statePaths: mDeps.StatePaths,
                    routeEvent: oRouteEvent,
                    owner: mIntentOptions && mIntentOptions.owner,
                    resumeMode: mIntentOptions && mIntentOptions.resumeMode
                });
            },
            clearPendingNavigationIntent: function () {
                runNavigationOperation("clearPendingIntent", {
                    stateModel: oStateModel,
                    statePaths: mDeps.StatePaths
                });
            },
            revertPendingNavigationIntent: function () {
                return runNavigationOperation("revertPendingIntent", {
                    component: oComponent,
                    stateModel: oStateModel,
                    statePaths: mDeps.StatePaths
                });
            },
            resumePendingNavigationIntent: function () {
                return runNavigationOperation("resumePendingIntent", {
                    component: oComponent,
                    stateModel: oStateModel,
                    statePaths: mDeps.StatePaths
                });
            },
            restorePendingNavigationIntent: function () {
                return runNavigationOperation("restorePendingIntent", {
                    component: oComponent,
                    stateModel: oStateModel,
                    statePaths: mDeps.StatePaths
                });
            }
        };
    }

    function attachRuntime(oComponent, mDeps, oRuntimeContext) {
        var mModels = oRuntimeContext.models || oRuntimeContext;
        var mServices = resolveRuntimeServices(oRuntimeContext);
        var mTelemetry = resolveRuntimeTelemetry(oRuntimeContext, mModels);
        var mHandlers = resolveRuntimeHandlers(oComponent, mServices, oRuntimeContext);
        var oRuntimeSupport = mServices.componentRuntimeSupport;

        attachRuntimeManagers(oComponent, mDeps, mModels, mHandlers, mTelemetry, mServices, oRuntimeSupport);
        initializeRouter(oComponent);
        return buildNavigationIntentApi(oComponent, mDeps, mModels.stateModel);
    }

    return {
        allSettledPolyfill: allSettledPolyfill,
        attachRuntime: attachRuntime,
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
