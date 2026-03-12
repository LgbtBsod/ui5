sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (ControllerViewStateRuntime, ModelStateRuntime, SchedulingRuntime, StatePaths, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var SEARCH_WORKING_HINT_MS = 2000;

    function clearSearchWorkingHintTimer(oController) {
        oController._iSearchWorkingHintTimer = SchedulingRuntime.clearTimer(oController._iSearchWorkingHintTimer);
    }

    function clearPendingSearchLoad(oController) {
        oController._iPendingSearchLoadTimer = SchedulingRuntime.clearTimer(oController._iPendingSearchLoadTimer);
        oController._oPendingSearchLoad = null;
    }

    function hideSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        ControllerViewStateRuntime.set(oController, "/filterHintVisible", false);
        ControllerViewStateRuntime.set(oController, "/filterHintText", "");
    }

    function isSearchLoading(oController) {
        return !!(
            ControllerViewStateRuntime.get(oController, "/tableBusy", false)
            || ControllerViewStateRuntime.get(oController, "/searchActionBusy", false)
            || ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, false)
        );
    }

    function scheduleSearchWorkingHint(oController) {
        clearSearchWorkingHintTimer(oController);
        oController._iSearchWorkingHintTimer = SchedulingRuntime.restartTimer(0, function () {
            var fnResolveWorkingText = oController && oController._resolveSearchWorkingText;
            if (!isSearchLoading(oController)) {
                return;
            }
            ControllerViewStateRuntime.set(oController, "/filterHintVisible", true);
            ControllerViewStateRuntime.set(oController, "/filterHintType", "Information");
            ControllerViewStateRuntime.set(
                oController,
                "/filterHintText",
                typeof fnResolveWorkingText === "function"
                    ? fnResolveWorkingText()
                    : "Working..."
            );
        }, SEARCH_WORKING_HINT_MS);
    }

    function beginSearchLoadingFeedback(oController) {
        scheduleSearchWorkingHint(oController);
    }

    function isSearchBindingSettled(oInnerTable) {
        var oBinding = oInnerTable && oInnerTable.getBinding && oInnerTable.getBinding("items");
        if (!oInnerTable || !oBinding) {
            return false;
        }
        if (typeof oInnerTable.getBusy === "function" && oInnerTable.getBusy()) {
            return false;
        }
        if (typeof oBinding.isPending === "function" && oBinding.isPending()) {
            return false;
        }
        if (oBinding.bPendingRequest || oBinding.bPendingRefresh) {
            return false;
        }
        if (typeof oBinding.isLengthFinal === "function") {
            return !!oBinding.isLengthFinal();
        }
        return true;
    }

    function bindPendingSearchLoad(oController, oInnerTable, mHooks) {
        var iStartedAt = Date.now();
        var fnPoll;
        clearPendingSearchLoad(oController);
        oController._oPendingSearchLoad = { settled: false };
        if (!oInnerTable || typeof oInnerTable.attachEventOnce !== "function") {
            return;
        }
        oInnerTable.attachEventOnce("updateFinished", function () {
            settlePendingSearchLoad(oController, { innerTable: oInnerTable }, mHooks);
        });
        fnPoll = function () {
            if (!oController._oPendingSearchLoad || oController._oPendingSearchLoad.settled) {
                return;
            }
            if (isSearchBindingSettled(oInnerTable) || (Date.now() - iStartedAt) >= 8000) {
                settlePendingSearchLoad(oController, { innerTable: oInnerTable }, mHooks);
                return;
            }
            oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(
                oController._iPendingSearchLoadTimer,
                fnPoll,
                250
            );
        };
        oController._iPendingSearchLoadTimer = SchedulingRuntime.restartTimer(0, fnPoll, 250);
    }

    function settlePendingSearchLoad(oController, oOptions, mHooks) {
        var oPending = oController._oPendingSearchLoad;
        var oInnerTable = oOptions && oOptions.innerTable;
        var oError = oOptions && oOptions.error;
        var sErrorMessage;
        var fnReadRows = mHooks && mHooks.readRows;
        var fnAfterSuccess = mHooks && mHooks.afterSuccess;
        var fnApplyLoadError = mHooks && mHooks.applyLoadError;
        var fnApplyLoadSuccess = mHooks && mHooks.applyLoadSuccess;
        if (!oPending || oPending.settled) {
            return;
        }
        oPending.settled = true;
        clearPendingSearchLoad(oController);
        hideSearchWorkingHint(oController);
        if (oError) {
            sErrorMessage = String((oError && (oError.message || oError.statusText)) || "Search request failed").trim();
            if (typeof fnApplyLoadError === "function") {
                fnApplyLoadError(sErrorMessage);
            }
            return;
        }
        if (typeof fnApplyLoadSuccess === "function") {
            fnApplyLoadSuccess(typeof fnReadRows === "function" ? fnReadRows(oInnerTable) : []);
        }
        if (typeof fnAfterSuccess === "function") {
            fnAfterSuccess(oInnerTable);
        }
    }

    return {
        beginSearchLoadingFeedback: beginSearchLoadingFeedback,
        bindPendingSearchLoad: bindPendingSearchLoad,
        clearPendingSearchLoad: clearPendingSearchLoad,
        clearSearchWorkingHintTimer: clearSearchWorkingHintTimer,
        hideSearchWorkingHint: hideSearchWorkingHint,
        settlePendingSearchLoad: settlePendingSearchLoad
    };
});
