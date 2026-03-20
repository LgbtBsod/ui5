sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/BackendModeContracts"
], function (Result, GatewayClient, ModelPathContracts, BackendModeContracts) {
    "use strict";

    /**
     * ZERO-LEGACY initialization.
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
                oStateModel.setProperty(BackendModeContracts.PATHS.BACKEND_MODE, BackendModeContracts.MODES.REAL);
                oStateModel.setProperty(BackendModeContracts.PATHS.BACKEND_SERVICE_URL, GatewayClient.serviceUrl() || "");
            }

            return Promise.resolve(Result.ok({ initialized: true }, []));
        }
    };
});
