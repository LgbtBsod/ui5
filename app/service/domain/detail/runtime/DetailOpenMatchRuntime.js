sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/OpenDetailUseCase"
], function (OpenDetailUseCase) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function DetailOpenMatchRuntime(mDeps) {
        var d = mDeps || {};
        this._openUseCase = d.openUseCase || OpenDetailUseCase();
    }

    DetailOpenMatchRuntime.prototype.open = function (mInput, mCtx) {
        return executeUseCase(this._openUseCase, mInput, mCtx);
    };

    return DetailOpenMatchRuntime;
});
