sap.ui.define([], function () {
    "use strict";

    function createMainServiceModel(oComponent, mDeps, sMainServiceUri) {
        var ODataModel = mDeps.ODataModel;
        var GatewayBackendService = mDeps.GatewayBackendService;
        var oMainServiceModel = oComponent.getModel("mainService") || new ODataModel(sMainServiceUri, {
            useBatch: true,
            tokenHandling: true,
            defaultBindingMode: "TwoWay",
            defaultCountMode: "Inline",
            refreshAfterChange: false
        });

        oMainServiceModel.setDeferredGroups(["changes", "autosave", "saveFlow", "locks"]);
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
