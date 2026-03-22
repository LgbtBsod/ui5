sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentPendingNavigationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFacadeEffectRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentForceReadOnlyRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentGuardedSaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCrossTabHandlerRuntime"
], function (
    ComponentPendingNavigationRuntime,
    ComponentFacadeEffectRuntime,
    ComponentForceReadOnlyRuntime,
    ComponentGuardedSaveRuntime,
    ComponentCrossTabHandlerRuntime
) {
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

    function createPendingNavigationRuntime(oComponent, oStateModel, StatePaths) {
        return ComponentPendingNavigationRuntime.createRuntime(oComponent, oStateModel, StatePaths);
    }

    function seedInitialState(oStateModel, StatePaths, TimeConfigService, ModelStateRuntime) {
        var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
        var mInitState = { "/timers": mTimerDefaults };
        mInitState[StatePaths.SAVE_IN_FLIGHT] = false;
        mInitState[StatePaths.PENDING_NAVIGATION_INTENT] = null;
        mInitState[StatePaths.TAB_CONFLICT_STATE] = { active: false, source: "", at: "" };
        mInitState["/networkOnline"] = true;
        mInitState["/networkGraceMode"] = false;
        mInitState["/networkGraceExpiresAt"] = null;
        ModelStateRuntime.setManyOnModel(oStateModel, mInitState);
        return mTimerDefaults;
    }

    function buildHandlerRuntime(oComponent, mDeps, mModels, oCoreRuntime, oFeedbackRuntime, oPendingNavigationRuntime, oComponentRuntimeSupport, oSaveGuardRuntime) {
        function readDirty() {
            return mDeps.ModelStateRuntime.readOnModel(mModels.stateModel, mDeps.StatePaths.WORKFLOW_DIRTY, false);
        }

        var fnHandleForceReadOnly = ComponentForceReadOnlyRuntime.createHandler({
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
        var fnRunGuardedSave = ComponentGuardedSaveRuntime.createHandler({
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
        var oCrossTabRuntime = ComponentCrossTabHandlerRuntime.register({
            component: oComponent,
            stateModel: mModels.stateModel,
            statePaths: mDeps.StatePaths,
            bundleText: mDeps.bundleText,
            setGlobalBanner: oFeedbackRuntime.setGlobalBanner,
            handleForceReadOnly: fnHandleForceReadOnly,
            attachCrossTabRuntime: mDeps.ComponentCrossTabRuntime.attachCrossTabRuntime
        }).crossTabRuntime;

        ComponentFacadeEffectRuntime.registerDefaultHandlers({
            actionDispatcher: oComponent._actionDispatcher,
            actionContract: mDeps.ActionContract,
            detailFacade: oComponent._detailFacade,
            runGuardedSave: fnRunGuardedSave,
            buildLatestCtx: oCoreRuntime.buildLatestCtx,
            applyFacadeResult: oCoreRuntime.applyFacadeResult,
            getCtx: function () {
                return oComponent._ctx;
            }
        });

        return {
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
        };
    }

    function bootstrap(oComponent, mDeps, mModels) {
        var oCoreRuntime = mDeps.ComponentCoreInitRuntime.initializeComponentRuntime(oComponent, mDeps, mModels);
        var oFeedbackRuntime = mDeps.ComponentFeedbackRuntime.createFeedbackRuntime({
            stateModel: mModels.stateModel,
            feedbackPolicy: mDeps.FeedbackPolicy,
            bundleText: mDeps.bundleText
        });
        var oRuntimeSettingsRuntime = initializeRuntimeSettings(oComponent, mDeps, mModels);
        var oPendingNavigationRuntime = createPendingNavigationRuntime(oComponent, mModels.stateModel, mDeps.StatePaths);
        var oComponentRuntimeSupport = mDeps.ComponentRuntimeSupport || mDeps.componentRuntimeSupport;
        var oSaveGuardRuntime = mDeps.ComponentSaveGuardRuntime || mDeps.saveGuardRuntime;
        var mHandlers = buildHandlerRuntime(
            oComponent,
            mDeps,
            mModels,
            oCoreRuntime,
            oFeedbackRuntime,
            oPendingNavigationRuntime,
            oComponentRuntimeSupport,
            oSaveGuardRuntime
        );
        var mTelemetry = {
            bundleText: mDeps.bundleText,
            emitTelemetry: mDeps.emitTelemetry,
            timerDefaults: seedInitialState(mModels.stateModel, mDeps.StatePaths, mDeps.TimeConfigService, mDeps.ModelStateRuntime)
        };

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
                applyFacadeResult: mHandlers.applyFacadeResult,
                buildLatestCtx: mHandlers.buildLatestCtx,
                clearGlobalBanner: mHandlers.clearGlobalBanner,
                clearPendingNavigationIntent: mHandlers.clearPendingNavigationIntent,
                handleForceReadOnly: mHandlers.handleForceReadOnly,
                publishTabSignal: mHandlers.publishTabSignal,
                queuePendingNavigationIntent: mHandlers.queuePendingNavigationIntent,
                resolveDetailCurrent: mHandlers.resolveDetailCurrent,
                restorePendingNavigationIntent: mHandlers.restorePendingNavigationIntent,
                resumePendingNavigationIntent: mHandlers.resumePendingNavigationIntent,
                revertPendingNavigationIntent: mHandlers.revertPendingNavigationIntent,
                runGuardedSave: mHandlers.runGuardedSave,
                setGlobalBanner: mHandlers.setGlobalBanner
            },
            telemetry: mTelemetry
        };
    }

    return {
        bootstrap: bootstrap
    };
});
