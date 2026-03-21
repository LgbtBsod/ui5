sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentInternalRuntimeState"
], function (ModelContracts, ComponentInternalRuntimeState) {
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
        var oInternalRuntimeState = oComponent._internalRuntimeState || {};
        var oExistingDetail = oComponent.getModel(MODELS.DETAIL);
        var oDetailModel = reuseJsonModel(oExistingDetail, ModelFactory.createDetailModel);

        return {
            stateModel: reuseJsonModel(oComponent.getModel(MODELS.STATE), ModelFactory.createStateModel),
            shellModel: reuseJsonModel(oComponent.getModel(MODELS.SHELL), ModelFactory.createShellModel),
            detailModel: oDetailModel,
            masterDataModel: reuseJsonModel(oComponent.getModel(MODELS.MASTER_DATA), ModelFactory.createMasterDataModel),
            cacheState: ComponentInternalRuntimeState.reuseState(oInternalRuntimeState.cache, ComponentInternalRuntimeState.createCacheState),
            envState: ComponentInternalRuntimeState.reuseState(oInternalRuntimeState.env, ComponentInternalRuntimeState.createEnvState),
            deviceModel: new JSONModel(Device)
        };
    }

    function registerModels(oComponent, mModels) {
        var oDeviceModel = mModels.deviceModel;
        oComponent._internalRuntimeState = {
            cache: mModels.cacheState,
            env: mModels.envState
        };
        oComponent.setModel(mModels.detailModel, MODELS.DETAIL);
        oComponent.setModel(mModels.stateModel, MODELS.STATE);
        oComponent.setModel(mModels.shellModel, MODELS.SHELL);
        oComponent.setModel(mModels.masterDataModel, MODELS.MASTER_DATA);
        oDeviceModel.setDefaultBindingMode("OneWay");
        oComponent.setModel(oDeviceModel, MODELS.DEVICE);
    }

    return {
        initializeModels: initializeModels,
        registerModels: registerModels
    };
});
