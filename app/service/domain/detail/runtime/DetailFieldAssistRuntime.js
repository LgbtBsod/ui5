sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/RowOpsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValueHelpLocationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/PersonSuggestUseCase"
], function (
    RowOpsUseCase,
    ValueHelpLocationUseCase,
    PersonSuggestUseCase
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function DetailFieldAssistRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            rowOps: d.rowOpsUseCase || RowOpsUseCase(),
            valueHelpLocation: d.valueHelpLocationUseCase || ValueHelpLocationUseCase(),
            personSuggest: d.personSuggestUseCase || PersonSuggestUseCase()
        };
    }

    DetailFieldAssistRuntime.prototype.rowOps = function (mInput, mCtx) {
        return executeUseCase(this._uc.rowOps, mInput, mCtx);
    };

    DetailFieldAssistRuntime.prototype.valueHelpLocation = function (mInput, mCtx) {
        return executeUseCase(this._uc.valueHelpLocation, mInput, mCtx);
    };

    DetailFieldAssistRuntime.prototype.personSuggest = function (mInput, mCtx) {
        return executeUseCase(this._uc.personSuggest, mInput, mCtx);
    };

    return DetailFieldAssistRuntime;
});
