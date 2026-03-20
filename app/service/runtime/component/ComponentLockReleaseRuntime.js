sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (StatePaths, CreateSentinel, GatewayClient, RootIdRuntime, LayoutStateRuntime, ModelStateRuntime, WorkflowContracts, GatewayContractConstants) {
    "use strict";

    function readActiveLockPayload(oStateModel) {
        var sRootId = RootIdRuntime.resolveActiveFromStateModel(oStateModel);
        var sSessionGuid = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "") || "").trim();
        var sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""), "");
        var sLockState = LayoutStateRuntime.normalizeState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, ""), "");
        if (!sRootId || !sSessionGuid || CreateSentinel.isCreateId(sRootId)) {
            return null;
        }
        if (sMode !== WorkflowContracts.EDIT_MODES.EDIT || sLockState !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
            return null;
        }
        return {
            RootId: sRootId,
            SessionGuid: sSessionGuid
        };
    }

    return {
        readActiveLockPayload: readActiveLockPayload,
        buildLockReleaseUrl: function (oStateModel) {
            var sServiceUrl = String(ModelStateRuntime.readOnModel(oStateModel, "/backendServiceUrl", "") || "").trim() || GatewayClient.serviceUrl();
            if (!sServiceUrl) {
                return "";
            }
            return String(sServiceUrl).replace(/\/+$/, "") + "/" + GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE;
        }
    };
});
