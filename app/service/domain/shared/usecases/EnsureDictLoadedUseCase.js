sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"
], function (Result) {
    "use strict";

    /**
     * Search init bundle loader.
     *
     * This use case owns only the search bootstrap dictionary/reference-data bundle.
     * App boot readiness stays in ComponentLifecycleRuntime; detail/analytics-specific data
     * must be loaded by their own route/domain flows.
     */
    return {
        execute: function (input, ctx) {
            var oStateModel = ctx && ctx.stateModel;

            if (!ctx || !ctx.dict || typeof ctx.dict.ensureLoaded !== "function") {
                return Promise.resolve(Result.fail({ code: "DICT_PORT_MISSING" }, []));
            }

            if (oStateModel && oStateModel.setProperty) {
                oStateModel.setProperty("/masterDataLoading", true);
            }

            return Promise.resolve(ctx.dict.ensureLoaded()).then(function () {
                if (oStateModel && oStateModel.setProperty) {
                    oStateModel.setProperty("/masterDataLoading", false);
                }
                return Result.ok({ loaded: true }, []);
            }).catch(function (e) {
                if (oStateModel && oStateModel.setProperty) {
                    oStateModel.setProperty("/masterDataLoading", false);
                }
                return Result.fail(e, []);
            });
        }
    };
});
