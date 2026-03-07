sap.ui.define([
    "sap_ui5/service/domain/analytics/usecases/LoadAnalyticsDashboardUseCase"
], function (LoadAnalyticsDashboardUseCase) {
    "use strict";

    function AnalyticsFacade(mDeps) {
        var d = mDeps || {};
        this._uc = {
            load: d.loadUseCase || new LoadAnalyticsDashboardUseCase()
        };
    }

    AnalyticsFacade.prototype.load = function (mInput, mCtx) {
        return this._uc.load.execute(mInput || {}, mCtx || {});
    };

    return AnalyticsFacade;
});
