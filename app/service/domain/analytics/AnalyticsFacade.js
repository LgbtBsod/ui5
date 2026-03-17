sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/usecases/LoadAnalyticsDashboardUseCase"
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

    AnalyticsFacade.prototype.destroy = function () {
        if (this._uc && this._uc.load) {
            if (typeof this._uc.load.destroy === "function") {
                this._uc.load.destroy();
            } else if (typeof this._uc.load.dispose === "function") {
                this._uc.load.dispose();
            }
        }
        this._uc = null;
    };

    return AnalyticsFacade;
});
