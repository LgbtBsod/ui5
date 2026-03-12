sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
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

    function createModels(oComponent, mDeps) {
        var ModelFactory = mDeps.ModelFactory;
        var JSONModel = mDeps.JSONModel;
        var Device = mDeps.Device;

        return {
            dataModel: reuseJsonModel(oComponent.getModel("data"), ModelFactory.createDataModel),
            mplModel: reuseJsonModel(oComponent.getModel("mpl"), ModelFactory.createMplModel),
            stateModel: reuseJsonModel(oComponent.getModel(MODELS.STATE), ModelFactory.createStateModel),
            uiStateModel: reuseJsonModel(oComponent.getModel("uiState"), ModelFactory.createUiStateModel),
            viewModel: reuseJsonModel(oComponent.getModel(MODELS.VIEW), ModelFactory.createViewModel),
            selectedModel: reuseJsonModel(oComponent.getModel(MODELS.SELECTED), function () { return new JSONModel({}); }),
            snapshotModel: reuseJsonModel(oComponent.getModel("snapshot"), ModelFactory.createSnapshotModel),
            masterDataModel: reuseJsonModel(oComponent.getModel(MODELS.MASTER_DATA), ModelFactory.createMasterDataModel),
            layoutModel: reuseJsonModel(oComponent.getModel("layout"), ModelFactory.createLayoutModel),
            cacheModel: reuseJsonModel(oComponent.getModel("cache"), ModelFactory.createCacheModel),
            envModel: ModelFactory.createEnvModel(),
            deviceModel: new JSONModel(Device)
        };
    }

    function registerModels(oComponent, mModels) {
        var oDeviceModel = mModels.deviceModel;
        oComponent.setModel(mModels.dataModel, "data");
        oComponent.setModel(mModels.mplModel, "mpl");
        oComponent.setModel(mModels.selectedModel, MODELS.SELECTED);
        oComponent.setModel(mModels.snapshotModel, "snapshot");
        oComponent.setModel(mModels.stateModel, MODELS.STATE);
        oComponent.setModel(mModels.uiStateModel, "uiState");
        oComponent.setModel(mModels.viewModel, MODELS.VIEW);
        oComponent.setModel(mModels.viewModel, MODELS.APP_VIEW);
        oComponent.setModel(mModels.masterDataModel, MODELS.MASTER_DATA);
        oComponent.setModel(mModels.layoutModel, "layout");
        oComponent.setModel(mModels.cacheModel, "cache");
        oComponent.setModel(mModels.envModel, "env");
        oDeviceModel.setDefaultBindingMode("OneWay");
        oComponent.setModel(oDeviceModel, "device");
    }

    return {
        createModels: createModels,
        registerModels: registerModels
    };
});
