sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/InitializeSearchUseCase"
], function (InitializeSearchUseCase) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function SearchBootstrapRuntime(mDeps) {
        var d = mDeps || {};
        this._bootstrapUseCase = d.bootstrapUseCase || InitializeSearchUseCase();
    }

    SearchBootstrapRuntime.prototype.bootstrap = function (mInput, mCtx) {
        return executeUseCase(this._bootstrapUseCase, mInput, mCtx);
    };

    return SearchBootstrapRuntime;
});
