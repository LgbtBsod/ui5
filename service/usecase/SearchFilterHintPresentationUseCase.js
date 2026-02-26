sap.ui.define([], function () {
    "use strict";

    function resolveHintState(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        var bUseSmartControls = !!(mArgs && mArgs.useSmartControls);
        var bHasSmartFilters = !!(mArgs && mArgs.hasSmartFilters);
        var mPayload = (mArgs && mArgs.fallbackPayload) || {};

        if (!oStateModel || typeof oStateModel.getProperty !== "function") {
            return {
                visible: false,
                textKey: "filterHintCleared",
                type: "Information",
                hasActiveFilters: false
            };
        }

        var bHasFallback = Boolean(String(mPayload.filterId || "").trim())
            || Boolean(mPayload.filterLpc)
            || mPayload.filterFailedChecks !== "ALL"
            || mPayload.filterFailedBarriers !== "ALL";

        var bHasActive = bUseSmartControls ? bHasSmartFilters : bHasFallback;

        if (bUseSmartControls && bHasSmartFilters) {
            return {
                visible: true,
                textKey: "filterHintSmartActive",
                type: "Information",
                hasActiveFilters: true
            };
        }

        if (!bUseSmartControls && bHasFallback) {
            return {
                visible: true,
                textKey: "filterHintFallbackActive",
                type: "Warning",
                hasActiveFilters: true
            };
        }

        return {
            visible: false,
            textKey: "filterHintCleared",
            type: "Success",
            hasActiveFilters: false
        };
    }

    function resolveHintText(mArgs) {
        var oBundle = mArgs && mArgs.bundle;
        var sKey = (mArgs && mArgs.textKey) || "filterHintCleared";
        if (!oBundle || typeof oBundle.getText !== "function") {
            return "";
        }
        try {
            return oBundle.getText(sKey);
        } catch (e) {
            return "";
        }
    }

    function applyHintPresentation(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return null;
        }

        var oState = resolveHintState(mArgs);
        var sText = resolveHintText({
            bundle: mArgs && mArgs.bundle,
            textKey: oState.textKey
        });

        oViewModel.setProperty("/hasActiveFilters", !!oState.hasActiveFilters);
        oViewModel.setProperty("/filterHintVisible", !!oState.visible);
        oViewModel.setProperty("/filterHintType", oState.type || "Information");
        oViewModel.setProperty("/filterHintText", sText || "");

        return {
            visible: !!oState.visible,
            type: oState.type || "Information",
            text: sText || "",
            hasActiveFilters: !!oState.hasActiveFilters,
            textKey: oState.textKey
        };
    }

    return {
        resolveHintState: resolveHintState,
        resolveHintText: resolveHintText,
        applyHintPresentation: applyHintPresentation
    };
});
