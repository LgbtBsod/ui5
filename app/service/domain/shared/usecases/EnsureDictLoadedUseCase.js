sap.ui.define([
    "checklist/app/service/framework/Result"
], function (Result) {
    "use strict";

    /**
     * ZERO-LEGACY dictionary bootstrap.
     *
     * Must use DictPort (infra/adapters/DictAdapter) and never call legacy BackendAdapter.
     */
    return {
        execute: function (input, ctx) {
            var oStateModel = ctx && ctx.stateModel;

            if (!ctx || !ctx.dict || typeof ctx.dict.ensureLoaded !== "function") {
                return Promise.resolve(Result.fail({ message: "DictPort missing: ctx.dict.ensureLoaded", code: "DICT_PORT_MISSING" }, []));
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
