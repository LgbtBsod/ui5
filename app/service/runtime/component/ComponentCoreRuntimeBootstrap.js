sap.ui.define([], function () {
    "use strict";

    function initializeRuntimeSettings(oComponent, mDeps, mModels) {
        var oRuntime = mDeps.ComponentRuntimeSettingsRuntime.initializeRuntimeSettings(oComponent, {
            stateModel: mModels.stateModel,
            envState: mModels.envState,
            masterDataModel: mModels.masterDataModel,
            settingsManager: mDeps.Managers && mDeps.Managers.SettingsManager || mDeps.SettingsManager,
            gatewayBackendService: mDeps.GatewayClient,
            telemetryRuntime: mDeps.TelemetryRuntime,
            emitTelemetry: mDeps.emitTelemetry
        });

        return {
            applyRuntimeSettings: oRuntime.applyRuntimeSettings,
            loadRuntimeSettings: function (mLoadOptions) {
                return oRuntime.loadRuntimeSettings(mLoadOptions).catch(function (oError) {
                    throw oError || new Error("runtime_settings_load_failed");
                });
            }
        };
    }

    function createPendingNavigationRuntime(mDeps, oComponent, oStateModel, StatePaths, resumePendingNavigationIntent) {
        return {
            queuePendingNavigationIntent: function (oRouteEvent, mIntentOptions) {
                return mDeps.ComponentActionRuntime.queuePendingNavigationIntent(oComponent, oStateModel, StatePaths, oRouteEvent, mIntentOptions);
            },
            clearPendingNavigationIntent: function () {
                return mDeps.ComponentActionRuntime.clearPendingNavigationIntent(oStateModel, StatePaths);
            },
            revertPendingNavigationIntent: function () {
                return mDeps.ComponentActionRuntime.revertPendingNavigationIntent(oComponent, oStateModel, StatePaths);
            },
            restorePendingNavigationIntent: function () {
                return mDeps.ComponentActionRuntime.restorePendingNavigationIntent(oComponent, oStateModel, StatePaths);
            },
            resumePendingNavigationIntent: function () {
                return resumePendingNavigationIntent(oComponent, oStateModel, StatePaths);
            }
        };
    }

    function seedInitialState(oStateModel, StatePaths, TimeConfigService) {
        var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
        var mInitState = { "/timers": mTimerDefaults };
        mInitState[StatePaths.SAVE_IN_FLIGHT] = false;
        mInitState[StatePaths.PENDING_NAVIGATION_INTENT] = null;
        mInitState[StatePaths.TAB_CONFLICT_STATE] = { active: false, source: "", at: "" };
        mInitState["/networkOnline"] = true;
        mInitState["/networkGraceMode"] = false;
        mInitState["/networkGraceExpiresAt"] = null;
        mDeps.ModelStateRuntime.setManyOnModel(oStateModel, mInitState);
        return mTimerDefaults;
    }

    function bootstrap(oComponent, mDeps, mModels) {
        var oCoreRuntime = mDeps.ComponentCoreInitRuntime.initializeComponentRuntime(oComponent, mDeps, mModels, {
            buildActionValidators: mDeps.ComponentActionRuntime.buildActionValidators,
            createApplyFacadeResult: mDeps.ComponentActionRuntime.createApplyFacadeResult
        });
        var oFeedbackRuntime = mDeps.ComponentFeedbackRuntime.createFeedbackRuntime({
            stateModel: mModels.stateModel,
            feedbackPolicy: mDeps.FeedbackPolicy,
            bundleText: mDeps.bundleText
        });
        var oRuntimeSettingsRuntime = initializeRuntimeSettings(oComponent, mDeps, mModels);
        var oPendingNavigationRuntime = createPendingNavigationRuntime(
            mDeps,
            oComponent,
            mModels.stateModel,
            mDeps.StatePaths,
            mDeps.ComponentActionRuntime.resumePendingNavigationIntent
        );
        var oComponentRuntimeSupport = mDeps.ComponentRuntimeSupport || mDeps.componentRuntimeSupport;
        var oSaveGuardRuntime = mDeps.ComponentSaveGuardRuntime || mDeps.saveGuardRuntime;

        function readDirty() {
            return mDeps.ModelStateRuntime.readOnModel(mModels.stateModel, mDeps.StatePaths.WORKFLOW_DIRTY, false);
        }

        var fnHandleForceReadOnly = mDeps.ComponentRuntimeHandlerRuntime.createForceReadOnlyHandler({
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            statePaths: mDeps.StatePaths,
            componentRuntimeSupport: oComponentRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime,
            applyFacadeResult: oCoreRuntime.applyFacadeResult,
            emitTelemetry: mDeps.emitTelemetry,
            clearPendingNavigationIntent: oPendingNavigationRuntime.clearPendingNavigationIntent,
            readDirty: readDirty
        });
        var fnRunGuardedSave = mDeps.ComponentRuntimeHandlerRuntime.createGuardedSave({
            component: oComponent,
            stateModel: mModels.stateModel,
            mainServiceModel: mModels.mainServiceModel,
            statePaths: mDeps.StatePaths,
            buildLatestCtx: oCoreRuntime.buildLatestCtx,
            applyFacadeResult: oCoreRuntime.applyFacadeResult,
            emitTelemetry: mDeps.emitTelemetry,
            resumePendingNavigationIntent: oPendingNavigationRuntime.resumePendingNavigationIntent,
            resolveCorrelationId: oFeedbackRuntime.resolveCorrelationId,
            isSessionExpiredError: oFeedbackRuntime.isSessionExpiredError,
            setGlobalBanner: oFeedbackRuntime.setGlobalBanner,
            clearGlobalBanner: oFeedbackRuntime.clearGlobalBanner,
            saveGuardRuntime: oSaveGuardRuntime
        });
        var oCrossTabRuntime = mDeps.ComponentRuntimeHandlerRuntime.registerCrossTabHandlers({
            component: oComponent,
            stateModel: mModels.stateModel,
            statePaths: mDeps.StatePaths,
            bundleText: mDeps.bundleText,
            setGlobalBanner: oFeedbackRuntime.setGlobalBanner,
            handleForceReadOnly: fnHandleForceReadOnly,
            attachCrossTabRuntime: mDeps.ComponentCrossTabRuntime.attachCrossTabRuntime
        }).crossTabRuntime;

        mDeps.ComponentRuntimeHandlerRuntime.registerDefaultHandlers({
            component: oComponent,
            actionContract: mDeps.ActionContract,
            runGuardedSave: fnRunGuardedSave,
            buildLatestCtx: oCoreRuntime.buildLatestCtx,
            applyFacadeResult: oCoreRuntime.applyFacadeResult,
            registerDefaultHandlers: mDeps.ComponentActionRuntime.registerDefaultHandlers
        });

        return {
            models: mModels,
            services: {
                componentRuntimeSupport: oComponentRuntimeSupport,
                feedbackRuntime: oFeedbackRuntime,
                runtimeSettingsRuntime: oRuntimeSettingsRuntime,
                pendingNavigationRuntime: oPendingNavigationRuntime,
                searchConfig: mDeps.SearchUiConfig.getLayoutSeed()
            },
            handlers: {
                applyFacadeResult: oCoreRuntime.applyFacadeResult,
                buildLatestCtx: oCoreRuntime.buildLatestCtx,
                clearGlobalBanner: oFeedbackRuntime.clearGlobalBanner,
                clearPendingNavigationIntent: oPendingNavigationRuntime.clearPendingNavigationIntent,
                handleForceReadOnly: fnHandleForceReadOnly,
                publishTabSignal: oCrossTabRuntime.publishTabSignal,
                queuePendingNavigationIntent: oPendingNavigationRuntime.queuePendingNavigationIntent,
                resolveDetailCurrent: oCoreRuntime.resolveDetailCurrent,
                restorePendingNavigationIntent: oPendingNavigationRuntime.restorePendingNavigationIntent,
                resumePendingNavigationIntent: oPendingNavigationRuntime.resumePendingNavigationIntent,
                revertPendingNavigationIntent: oPendingNavigationRuntime.revertPendingNavigationIntent,
                runGuardedSave: fnRunGuardedSave,
                setGlobalBanner: oFeedbackRuntime.setGlobalBanner
            },
            telemetry: {
                bundleText: mDeps.bundleText,
                emitTelemetry: mDeps.emitTelemetry,
                timerDefaults: seedInitialState(mModels.stateModel, mDeps.StatePaths, mDeps.TimeConfigService)
            }
        };
    }

    return {
        bootstrap: bootstrap
    };
});
