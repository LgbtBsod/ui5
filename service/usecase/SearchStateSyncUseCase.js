sap.ui.define([
    "sap_ui5/service/usecase/SearchSelectionNavigationUseCase",
    "sap_ui5/service/usecase/SearchToolbarActionStateUseCase"
], function (SearchSelectionNavigationUseCase, SearchToolbarActionStateUseCase) {
    "use strict";

    function syncSelectionAndActionState(mDeps) {
        if (!mDeps || !mDeps.viewModel || !mDeps.dataModel || !mDeps.selectedModel) {
            return { ok: false, reason: "missing_dependency" };
        }

        SearchSelectionNavigationUseCase.syncSelectionState({
            selectedModel: mDeps.selectedModel,
            dataModel: mDeps.dataModel,
            viewModel: mDeps.viewModel
        });

        SearchToolbarActionStateUseCase.applyActionStateToViewModel({
            selectedModel: mDeps.selectedModel,
            dataModel: mDeps.dataModel,
            viewModel: mDeps.viewModel,
            isLoading: !!mDeps.isLoading,
            useSmartControls: !!mDeps.useSmartControls
        });

        return { ok: true, reason: "synced" };
    }

    function applyRouteMatchedDefaults(oStateModel) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return { ok: false, reason: "missing_state_model_adapter" };
        }
        oStateModel.setProperty("/layout", "OneColumn");
        oStateModel.setProperty("/mode", "READ");
        return { ok: true, reason: "applied" };
    }

    return {
        syncSelectionAndActionState: syncSelectionAndActionState,
        applyRouteMatchedDefaults: applyRouteMatchedDefaults
    };
});
