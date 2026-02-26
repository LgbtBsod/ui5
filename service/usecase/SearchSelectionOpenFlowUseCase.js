sap.ui.define([
    "sap_ui5/service/usecase/SearchSelectionHydrationUseCase",
    "sap_ui5/service/usecase/SearchOpenDetailGuardUseCase",
    "sap_ui5/service/usecase/SearchSelectionLifecycleUseCase",
    "sap_ui5/service/usecase/SearchNavigationIntentUseCase",
    "sap_ui5/service/usecase/SearchActionMessagePresentationUseCase"
], function (
    SearchSelectionHydrationUseCase,
    SearchOpenDetailGuardUseCase,
    SearchSelectionLifecycleUseCase,
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


    function runSelectionChange(mDeps) {
        return SearchSelectionLifecycleUseCase.runSelectionChangeLifecycle({
            event: mDeps && mDeps.event,
            extractId: mDeps && mDeps.extractId,
            hydrateSelection: function (sId) {
                return hydrateSelection({
                    id: sId,
                    selectedModel: mDeps && mDeps.selectedModel,
                    viewModel: mDeps && mDeps.viewModel,
                    loadChecklistById: mDeps && mDeps.loadChecklistById,
                    syncSelectionState: mDeps && mDeps.syncSelectionState
                });
            }
        });
    }

    function runItemPress(mDeps) {
        return SearchSelectionLifecycleUseCase.runItemPressLifecycle({
            event: mDeps && mDeps.event,
            extractId: mDeps && mDeps.extractId,
            hydrateSelection: function (sId) {
                return hydrateSelection({
                    id: sId,
                    selectedModel: mDeps && mDeps.selectedModel,
                    viewModel: mDeps && mDeps.viewModel,
                    loadChecklistById: mDeps && mDeps.loadChecklistById,
                    syncSelectionState: mDeps && mDeps.syncSelectionState
                });
            },
            openDetail: function (sId) {
                return openDetail({
                    id: sId,
                    confirmNavigation: mDeps && mDeps.confirmNavigation,
                    stateModel: mDeps && mDeps.stateModel,
                    navTo: mDeps && mDeps.navTo,
                    bundle: mDeps && mDeps.bundle,
                    showToast: mDeps && mDeps.showToast
                });
            }
        });
    }

    function buildSelectionInteractionArgs(mDeps) {
        return {
            event: mDeps && mDeps.event,
            extractId: mDeps && mDeps.extractId,
            selectedModel: mDeps && mDeps.selectedModel,
            viewModel: mDeps && mDeps.viewModel,
            loadChecklistById: mDeps && mDeps.loadChecklistById,
            syncSelectionState: mDeps && mDeps.syncSelectionState,
            confirmNavigation: mDeps && mDeps.confirmNavigation,
            stateModel: mDeps && mDeps.stateModel,
            navTo: mDeps && mDeps.navTo,
            bundle: mDeps && mDeps.bundle,
            showToast: mDeps && mDeps.showToast
        };
    }

    function runSelectionInteractionOrchestration(mDeps) {
        var sSourceType = (mDeps && mDeps.sourceType) || "fallback";
        var sKind = (mDeps && mDeps.kind) || "selectionChange";
        var fnExtractId = sSourceType === "smart"
            ? mDeps && mDeps.extractIdFromSmartEvent
            : mDeps && mDeps.extractIdFromFallbackEvent;

        var oArgs = buildSelectionInteractionArgs({
            event: mDeps && mDeps.event,
            extractId: fnExtractId,
            selectedModel: mDeps && mDeps.selectedModel,
            viewModel: mDeps && mDeps.viewModel,
            loadChecklistById: mDeps && mDeps.loadChecklistById,
            syncSelectionState: mDeps && mDeps.syncSelectionState,
            confirmNavigation: mDeps && mDeps.confirmNavigation,
            stateModel: mDeps && mDeps.stateModel,
            navTo: mDeps && mDeps.navTo,
            bundle: mDeps && mDeps.bundle,
            showToast: mDeps && mDeps.showToast
        });

        if (sKind === "itemPress") {
            return runItemPress(oArgs);
        }
        return runSelectionChange(oArgs);
    }


    return {
        hydrateSelection: hydrateSelection,
        openDetail: openDetail,
        runSelectionChange: runSelectionChange,
        runItemPress: runItemPress,
        buildSelectionInteractionArgs: buildSelectionInteractionArgs,
        runSelectionInteractionOrchestration: runSelectionInteractionOrchestration
    };
});
