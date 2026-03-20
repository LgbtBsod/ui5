sap.ui.define([
    "sap/ui/model/odata/v2/ODataModel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (ODataModel, WorkflowRuntimeConstants) {
    "use strict";

    function createMainServiceModel(oComponent, mDeps, sMainServiceUri) {
        var GatewayClient = mDeps.GatewayClient;
        var mChangeGroups = {
            "*": {
                groupId: WorkflowRuntimeConstants.REQUEST_GROUPS.CHANGES,
                changeSetId: "ChecklistSave",
                single: false
            },
            "LockAcquireType": { groupId: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS, single: true },
            "LockHeartbeatType": { groupId: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS, single: true },
            "LockReleaseType": { groupId: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS, single: true }
        };
        var oMainServiceModel = new ODataModel(sMainServiceUri, {
            useBatch: true,
            tokenHandling: true,
            defaultBindingMode: "OneWay",
            defaultCountMode: "Inline",
            refreshAfterChange: false,
            defaultOperationMode: "Server",
            deferredGroups: [
                WorkflowRuntimeConstants.REQUEST_GROUPS.CHANGES,
                WorkflowRuntimeConstants.REQUEST_GROUPS.AUTOSAVE,
                WorkflowRuntimeConstants.REQUEST_GROUPS.SAVE_FLOW,
                WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS
            ],
            changeGroups: mChangeGroups,
            updateMethod: "MERGE"
        });

        oMainServiceModel.setDefaultBindingMode("OneWay");
        oMainServiceModel.setChangeGroups(mChangeGroups);
        oComponent.setModel(oMainServiceModel, "mainService");
        oComponent.setModel(oMainServiceModel);
        GatewayClient.setModel(oMainServiceModel, { serviceUrl: sMainServiceUri });

        return oMainServiceModel;
    }

    return {
        createMainServiceModel: createMainServiceModel
    };
});
