sap.ui.define([], function () {
    "use strict";

    function createInitContext(oComponent, mDeps, mModels, mOptions) {
        return {
            bundleText: mOptions.bundleText,
            emitTelemetry: mOptions.emitTelemetry,
            feedbackRuntime: mOptions.feedbackBootstrap.createFeedbackRuntime({
                stateModel: mModels.stateModel,
                feedbackPolicy: mDeps.FeedbackPolicy,
                bundleText: mOptions.bundleText
            }),
            runtimeSettingsRuntime: mOptions.feedbackBootstrap.initializeRuntimeSettings(oComponent, {
                stateModel: mModels.stateModel,
                envModel: mModels.envModel,
                masterDataModel: mModels.masterDataModel,
                settingsManager: mDeps.Managers && mDeps.Managers.SettingsManager || mDeps.SettingsManager,
                gatewayBackendService: mDeps.GatewayBackendService,
                telemetryRuntime: mDeps.TelemetryRuntime,
                emitTelemetry: mOptions.emitTelemetry
            }),
            pendingNavigationRuntime: mOptions.feedbackBootstrap.createPendingNavigationRuntime(
                oComponent,
                mModels.stateModel,
                mDeps.StatePaths,
                mOptions.resumePendingNavigationIntent
            )
        };
    }

    return {
        createInitContext: createInitContext
    };
});
