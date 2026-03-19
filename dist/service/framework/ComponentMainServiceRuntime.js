sap.ui.define([], function () {
    "use strict";

    function createMainServiceModel(oComponent, mDeps, sMainServiceUri) {
        var GatewayBackendService = mDeps.GatewayBackendService;
        var oMainServiceModel = oComponent.getModel("mainService");

        if (!oMainServiceModel) {
            throw new Error("Manifest-driven mainService model is missing. Check sap.ui5/models/mainService in manifest.json.");
        }

        oMainServiceModel.setDefaultBindingMode("OneWay");
        oMainServiceModel.setChangeGroups({
            "*": {
                groupId: "changes",
                changeSetId: "ChecklistSave",
                single: false
            },
            "LockAcquireType": { groupId: "locks", single: true },
            "LockHeartbeatType": { groupId: "locks", single: true },
            "LockReleaseType": { groupId: "locks", single: true }
        });

        oComponent.setModel(oMainServiceModel, "mainService");
        oComponent.setModel(oMainServiceModel);
        GatewayBackendService.setModel(oMainServiceModel, { serviceUrl: sMainServiceUri });

        return oMainServiceModel;
    }

    return {
        createMainServiceModel: createMainServiceModel
    };
});
