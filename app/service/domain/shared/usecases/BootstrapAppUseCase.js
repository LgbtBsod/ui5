sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayBackendService",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (Result, GatewayBackendService, ModelPathContracts) {
    "use strict";

    /**
     * ZERO-LEGACY bootstrap.
     *
     * The application must not depend on any legacy BackendAdapter/login/init flows.
     * Real SAP Gateway readiness is achieved by relying on ODataModel metadata + domain usecases.
     */
    return {
        execute: function (input, ctx) {
            var oStateModel = ctx && ctx.stateModel;

            if (oStateModel && oStateModel.setProperty) {
                oStateModel.setProperty(ModelPathContracts.UI_BUSY_GLOBAL, false);
                oStateModel.setProperty("/locationsLoading", false);
                oStateModel.setProperty("/backendMode", "real");
                oStateModel.setProperty("/backendServiceUrl", GatewayBackendService.serviceUrl() || "");
            }

            return Promise.resolve(Result.ok({ bootstrapped: true }, []));
        }
    };
});
