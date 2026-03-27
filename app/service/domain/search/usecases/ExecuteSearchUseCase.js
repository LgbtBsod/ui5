sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchSelectionEffects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts"
], function (Result, Effects, SearchSelectionEffects, StatePaths, ModelContracts, SearchRuntimeConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var SEARCH_MODE = SearchRuntimeConstants.SEARCH_MODE;

    function ExecuteSearchUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function execute(mInput, mCtx) {
        var oSmartControls = mCtx && mCtx.smartControls;
        if (!oSmartControls || typeof oSmartControls.rebindSearchTable !== "function") {
            return Promise.resolve(Result.fail({ code: "SMART_CONTROLS_UNAVAILABLE" }));
        }

        var sIntent = (mInput && mInput.intent) || "search";
        var aEffects = [Effects.modelPatch(VIEW_MODEL, "/hasSearched", true)].concat(
            SearchSelectionEffects.buildSelectionResetEffects({ markBusy: true })
        );

        if (sIntent === "searchModeToggle") {
            aEffects.push(Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_MODE, (mInput && mInput.state) ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT));
        }

        if (typeof oSmartControls.isReady === "function" && !oSmartControls.isReady()) {
            return Promise.resolve(Result.ok({ intent: sIntent, skipped: true, reason: "SMART_CONTROLS_NOT_READY" }, aEffects.filter(function (oEffect) {
                return oEffect.path !== StatePaths.UI_BUSY_SEARCH_TABLE;
            })));
        }

        oSmartControls.rebindSearchTable();

        return Promise.resolve(Result.ok({ intent: sIntent }, aEffects));
    }

    return ExecuteSearchUseCase;
});
