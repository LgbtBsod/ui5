sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (StatePaths, CreateSentinel, GatewayClient, LayoutStateRuntime, ModelStateRuntime, WorkflowContracts, GatewayContractConstants) {
    "use strict";

    function readActiveLockPayload(oStateModel) {
        var sRootId = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") || "").trim();
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
            DB_KEY: sRootId,
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
