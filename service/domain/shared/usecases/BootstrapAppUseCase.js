sap.ui.define([
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/backend/GatewayBackendService"
], function (Result, GatewayBackendService) {
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
                oStateModel.setProperty("/isLoading", true);
                oStateModel.setProperty("/masterDataLoading", true);
                oStateModel.setProperty("/locationsLoading", false);
                oStateModel.setProperty("/backendMode", "real");
                oStateModel.setProperty("/backendServiceUrl", GatewayBackendService.serviceUrl() || "");
            }

            return Promise.resolve(Result.ok({ bootstrapped: true }, [])).finally(function () {
                if (oStateModel && oStateModel.setProperty) {
                    oStateModel.setProperty("/isLoading", false);
                }
            });
        }
    };
});
