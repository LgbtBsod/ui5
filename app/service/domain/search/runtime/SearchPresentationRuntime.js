sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExportSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/AnalyticsUseCase"
], function (ExportSearchUseCase, AnalyticsUseCase) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function SearchPresentationRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            exportFlow: d.exportUseCase || ExportSearchUseCase(),
            analytics: d.analyticsUseCase || AnalyticsUseCase()
        };
    }

    SearchPresentationRuntime.prototype.exportFlow = function (mInput, mCtx) {
        return executeUseCase(this._uc.exportFlow, mInput, mCtx);
    };

    SearchPresentationRuntime.prototype.analytics = function (mInput, mCtx) {
        return executeUseCase(this._uc.analytics, mInput, mCtx);
    };

    return SearchPresentationRuntime;
});
