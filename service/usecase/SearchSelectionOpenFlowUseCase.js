sap.ui.define([
    "sap_ui5/service/usecase/SearchSelectionHydrationUseCase",
    "sap_ui5/service/usecase/SearchOpenDetailGuardUseCase",
    "sap_ui5/service/usecase/SearchNavigationIntentUseCase",
    "sap_ui5/service/usecase/SearchActionMessagePresentationUseCase"
], function (
    SearchSelectionHydrationUseCase,
    SearchOpenDetailGuardUseCase,
    SearchNavigationIntentUseCase,
    SearchActionMessagePresentationUseCase
) {
    "use strict";

    function hydrateSelection(mDeps) {
        return SearchSelectionHydrationUseCase.runSelectionHydration({
            id: mDeps && mDeps.id,
            selectedModel: mDeps && mDeps.selectedModel,
            viewModel: mDeps && mDeps.viewModel,
            loadChecklistById: mDeps && mDeps.loadChecklistById
        }).then(function (oResult) {
            if (typeof (mDeps && mDeps.syncSelectionState) === "function") {
                mDeps.syncSelectionState();
            }
            return {
                ok: true,
                checklist: (oResult && oResult.checklist) || null,
                reason: (oResult && oResult.reason) || "hydrated"
            };
        });
    }

    function openDetail(mDeps) {
        return SearchOpenDetailGuardUseCase.runOpenDetailFlow({
            id: mDeps && mDeps.id,
            confirmNavigation: mDeps && mDeps.confirmNavigation,
            buildIntent: function (sIntentId) {
                return SearchNavigationIntentUseCase.buildOpenDetailIntent({ id: sIntentId });
            },
            applyIntent: function (mIntent) {
                return SearchNavigationIntentUseCase.applyIntent({
                    intent: mIntent,
                    stateModel: mDeps && mDeps.stateModel,
                    navTo: mDeps && mDeps.navTo
                });
            }
        }).then(function (oResult) {
            if (oResult && oResult.reason === "missing_id") {
                SearchActionMessagePresentationUseCase.presentMissingChecklistId({
                    bundle: mDeps && mDeps.bundle,
                    showToast: mDeps && mDeps.showToast
                });
            }
            return {
                ok: !!(oResult && oResult.ok),
                reason: (oResult && oResult.reason) || "no_result"
            };
        });
    }

    return {
        hydrateSelection: hydrateSelection,
        openDetail: openDetail
    };
});
