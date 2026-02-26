sap.ui.define([
    "sap_ui5/service/usecase/SearchCreateCopyNavigationGuardUseCase",
    "sap_ui5/service/usecase/SearchNavigationIntentUseCase",
    "sap_ui5/service/usecase/SearchActionMessagePresentationUseCase"
], function (
    SearchCreateCopyNavigationGuardUseCase,
    SearchNavigationIntentUseCase,
    SearchActionMessagePresentationUseCase
) {
    "use strict";

    function runCreateFlow(mDeps) {
        return SearchCreateCopyNavigationGuardUseCase.runCreateNavigationFlow({
            confirmNavigation: mDeps && mDeps.confirmNavigation,
            buildCreateIntent: SearchNavigationIntentUseCase.buildCreateIntent,
            applyIntent: function (mIntent) {
                return SearchNavigationIntentUseCase.applyIntent({
                    intent: mIntent,
                    stateModel: mDeps && mDeps.stateModel,
                    navTo: mDeps && mDeps.navTo
                });
            }
        }).then(function (oResult) {
            return { ok: !!(oResult && oResult.ok), reason: (oResult && oResult.reason) || "no_result" };
        });
    }

    function runCopyFlow(mDeps) {
        return SearchCreateCopyNavigationGuardUseCase.runCopyNavigationFlow({
            resolveSelectedId: function () {
                return SearchNavigationIntentUseCase.resolveSelectedId({ selectedModel: mDeps && mDeps.selectedModel });
            },
            confirmNavigation: mDeps && mDeps.confirmNavigation,
            buildCopyIntent: function (sSelectedId) {
                return SearchNavigationIntentUseCase.buildCopyIntent({ selectedId: sSelectedId });
            },
            applyIntent: function (mIntent) {
                return SearchNavigationIntentUseCase.applyIntent({
                    intent: mIntent,
                    stateModel: mDeps && mDeps.stateModel,
                    navTo: mDeps && mDeps.navTo
                });
            }
        }).then(function (oResult) {
            if (oResult && oResult.reason === "missing_selection") {
                SearchActionMessagePresentationUseCase.presentCopyMissingSelection({
                    bundle: mDeps && mDeps.bundle,
                    showToast: mDeps && mDeps.showToast
                });
            }
            return { ok: !!(oResult && oResult.ok), reason: (oResult && oResult.reason) || "no_result" };
        });
    }

    return {
        runCreateFlow: runCreateFlow,
        runCopyFlow: runCopyFlow
    };
});
