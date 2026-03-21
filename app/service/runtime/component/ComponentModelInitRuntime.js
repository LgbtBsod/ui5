sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

    function reuseJsonModel(oExistingModel, fnCreateModel) {
        var oModel = oExistingModel || fnCreateModel();
        var oSeedModel;

        if (oExistingModel && typeof oExistingModel.setData === "function") {
            oSeedModel = fnCreateModel();
            oExistingModel.setData(oSeedModel && oSeedModel.getData ? oSeedModel.getData() : {}, false);
        }

        return oModel;
    }

    function initializeModels(oComponent, mDeps) {
        var ModelFactory = mDeps.ModelFactory;
        var JSONModel = mDeps.JSONModel;
        var Device = mDeps.Device;

        return {
            locationTreeModel: reuseJsonModel(oComponent.getModel(MODELS.LOCATION_TREE), ModelFactory.createLocationTreeModel),
            stateModel: reuseJsonModel(oComponent.getModel(MODELS.STATE), ModelFactory.createStateModel),
            shellModel: reuseJsonModel(oComponent.getModel(MODELS.SHELL), ModelFactory.createShellModel),
            viewModel: reuseJsonModel(oComponent.getModel(MODELS.VIEW), ModelFactory.createViewModel),
            selectedModel: reuseJsonModel(oComponent.getModel(MODELS.SELECTED), function () { return new JSONModel({}); }),
            snapshotModel: reuseJsonModel(oComponent.getModel(MODELS.SNAPSHOT), ModelFactory.createSnapshotModel),
            masterDataModel: reuseJsonModel(oComponent.getModel(MODELS.MASTER_DATA), ModelFactory.createMasterDataModel),
            cacheModel: reuseJsonModel(oComponent.getModel(MODELS.CACHE), ModelFactory.createCacheModel),
            envModel: ModelFactory.createEnvModel(),
            deviceModel: new JSONModel(Device)
        };
    }

    function registerModels(oComponent, mModels) {
        var oDeviceModel = mModels.deviceModel;
        oComponent.setModel(mModels.locationTreeModel, MODELS.LOCATION_TREE);
        oComponent.setModel(mModels.selectedModel, MODELS.SELECTED);
        oComponent.setModel(mModels.snapshotModel, MODELS.SNAPSHOT);
        oComponent.setModel(mModels.stateModel, MODELS.STATE);
        oComponent.setModel(mModels.shellModel, MODELS.SHELL);
        oComponent.setModel(mModels.viewModel, MODELS.VIEW);
        oComponent.setModel(mModels.masterDataModel, MODELS.MASTER_DATA);
        oComponent.setModel(mModels.cacheModel, MODELS.CACHE);
        oComponent.setModel(mModels.envModel, MODELS.ENV);
        oDeviceModel.setDefaultBindingMode("OneWay");
        oComponent.setModel(oDeviceModel, MODELS.DEVICE);
    }

    return {
        initializeModels: initializeModels,
        registerModels: registerModels
    };
});
