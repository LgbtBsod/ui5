sap.ui.define([], function () {
    "use strict";

    function createHandler(mOptions) {
        var oSaveGuardRuntime = mOptions.saveGuardRuntime || mOptions.saveGuardSupport;

        return oSaveGuardRuntime.createRunGuardedSave({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            mainServiceModel: mOptions.mainServiceModel,
            statePaths: mOptions.statePaths,
            detailFacade: mOptions.component._detailFacade,
            buildLatestCtx: mOptions.buildLatestCtx,
            applyFacadeResult: mOptions.applyFacadeResult,
            emitTelemetry: mOptions.emitTelemetry,
            resumePendingNavigationIntent: mOptions.resumePendingNavigationIntent,
            resolveCorrelationId: mOptions.resolveCorrelationId,
            isSessionExpiredError: mOptions.isSessionExpiredError,
            setGlobalBanner: mOptions.setGlobalBanner,
            clearGlobalBanner: mOptions.clearGlobalBanner
        });
    }

    return {
        createHandler: createHandler
    };
});
