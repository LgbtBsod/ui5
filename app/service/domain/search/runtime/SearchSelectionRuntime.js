sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectRowUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectionChangedUseCase"
], function (SelectRowUseCase, SelectionChangedUseCase) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function SearchSelectionRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            selectRow: d.selectRowUseCase || SelectRowUseCase(),
            selectionChanged: d.selectionChangedUseCase || SelectionChangedUseCase()
        };
    }

    SearchSelectionRuntime.prototype.selectRow = function (mInput, mCtx) {
        return executeUseCase(this._uc.selectRow, mInput, mCtx);
    };

    SearchSelectionRuntime.prototype.selectionChanged = function (mInput, mCtx) {
        return executeUseCase(this._uc.selectionChanged, mInput, mCtx);
    };

    return SearchSelectionRuntime;
});
