sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchSelectionEffects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Result, Effects, SearchSelectionEffects, StatePaths, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function RebindSearchUseCase() {
        return {
            execute: execute
        };
    }

    function execute(mInput, mCtx) {
        var oSmartControls = mCtx && mCtx.smartControls;
        if (!oSmartControls || typeof oSmartControls.rebindSearchTable !== "function") {
            return Promise.resolve(Result.fail({ code: "PORT_UNAVAILABLE" }, [Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, false)]));
        }

        if (typeof oSmartControls.isReady === "function" && !oSmartControls.isReady()) {
            return Promise.resolve(Result.ok({ reason: "smartControlsNotReady", skipped: true }, []));
        }

        oSmartControls.rebindSearchTable();

        return Promise.resolve(Result.ok(
            { reason: (mInput && mInput.source) || "rebind" },
            SearchSelectionEffects.buildSelectionResetEffects({ markBusy: true })
        ));
    }

    return RebindSearchUseCase;
});
