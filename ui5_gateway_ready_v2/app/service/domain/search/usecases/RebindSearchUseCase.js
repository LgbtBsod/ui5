sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/search/SearchSelectionEffects",
    "checklist/app/service/domain/shared/StatePaths"
], function (UseCase, Result, Effects, SearchSelectionEffects, StatePaths) {
    "use strict";

    function RebindSearchUseCase() {
        UseCase.call(this, "RebindSearchUseCase");
    }

    RebindSearchUseCase.prototype = Object.create(UseCase.prototype);
    RebindSearchUseCase.prototype.constructor = RebindSearchUseCase;

    RebindSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var oSmartControls = mCtx && mCtx.smartControls;
        if (!oSmartControls || typeof oSmartControls.rebindSearchTable !== "function") {
            return Promise.resolve(Result.fail({ message: "SmartControls unavailable", code: "PORT_UNAVAILABLE" }, [Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false)]));
        }

        if (typeof oSmartControls.isReady === "function" && !oSmartControls.isReady()) {
            return Promise.resolve(Result.ok({ reason: "smartControlsNotReady", skipped: true }, []));
        }

        oSmartControls.rebindSearchTable();

        return Promise.resolve(Result.ok(
            { reason: (mInput && mInput.source) || "rebind" },
            SearchSelectionEffects.buildSelectionResetEffects({ markBusy: true })
        ));
    };

    return RebindSearchUseCase;
});
