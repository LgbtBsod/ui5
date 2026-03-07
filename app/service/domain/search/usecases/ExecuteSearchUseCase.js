sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/search/SearchSelectionEffects",
    "sap_ui5/service/domain/shared/StatePaths"
], function (UseCase, Result, Effects, SearchSelectionEffects, StatePaths) {
    "use strict";

    function ExecuteSearchUseCase() {
        UseCase.call(this, "ExecuteSearchUseCase");
    }

    ExecuteSearchUseCase.prototype = Object.create(UseCase.prototype);
    ExecuteSearchUseCase.prototype.constructor = ExecuteSearchUseCase;

    ExecuteSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var oSmartControls = mCtx && mCtx.smartControls;
        if (!oSmartControls || typeof oSmartControls.rebindSearchTable !== "function") {
            return Promise.resolve(Result.fail({ message: "Smart controls unavailable", code: "SMART_CONTROLS_UNAVAILABLE" }));
        }

        var sIntent = (mInput && mInput.intent) || "search";
        var aEffects = [Effects.modelPatch("view", "/hasSearched", true)].concat(
            SearchSelectionEffects.buildSelectionResetEffects({ markBusy: true })
        );

        if (sIntent === "searchModeToggle") {
            aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_MODE, (mInput && mInput.state) ? "LOOSE" : "EXACT"));
        }

        if (typeof oSmartControls.isReady === "function" && !oSmartControls.isReady()) {
            return Promise.resolve(Result.ok({ intent: sIntent, skipped: true, reason: "SMART_CONTROLS_NOT_READY" }, aEffects.filter(function (oEffect) {
                return oEffect.path !== StatePaths.UI_BUSY_SEARCH_TABLE;
            })));
        }

        oSmartControls.rebindSearchTable();

        return Promise.resolve(Result.ok({ intent: sIntent }, aEffects));
    };

    return ExecuteSearchUseCase;
});
