sap.ui.define([
    "sap/ui/model/odata/v2/ODataModel"
], function (ODataModel) {
    "use strict";

    function createMainServiceModel(oComponent, mDeps, sMainServiceUri) {
        var GatewayBackendService = mDeps.GatewayBackendService;
        var mChangeGroups = {
            "*": {
                groupId: "changes",
                changeSetId: "ChecklistSave",
                single: false
            },
            "LockAcquireType": { groupId: "locks", single: true },
            "LockHeartbeatType": { groupId: "locks", single: true },
            "LockReleaseType": { groupId: "locks", single: true }
        };
        var oMainServiceModel = new ODataModel(sMainServiceUri, {
            useBatch: true,
            tokenHandling: true,
            defaultBindingMode: "OneWay",
            defaultCountMode: "Inline",
            refreshAfterChange: false,
            defaultOperationMode: "Server",
            deferredGroups: ["changes", "autosave", "saveFlow", "locks"],
            changeGroups: mChangeGroups,
            updateMethod: "MERGE"
        });

        oMainServiceModel.setDefaultBindingMode("OneWay");
        oMainServiceModel.setChangeGroups(mChangeGroups);
        oComponent.setModel(oMainServiceModel, "mainService");
        oComponent.setModel(oMainServiceModel);
        GatewayBackendService.setModel(oMainServiceModel, { serviceUrl: sMainServiceUri });

        return oMainServiceModel;
    }

    return {
        createMainServiceModel: createMainServiceModel
    };
});
