sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (JSONModel, ModelStateRuntime, ControllerModelRuntime, ModelContracts) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function initModel(oController, vState) {
        var vData = typeof vState === "function" ? vState() : vState;
        var oModel = ControllerModelRuntime.viewState(oController);
        if (oModel && typeof oModel.setData === "function") {
            oModel.setData(vData || {});
            return oModel;
        }
        if (oController && typeof oController.setModel === "function") {
            oController.setModel(new JSONModel(vData || {}), VIEW_MODEL);
        }
        return ControllerModelRuntime.viewState(oController);
    }

    function get(oController, sPath, vFallback) {
        return ModelStateRuntime.read(oController, VIEW_MODEL, sPath, vFallback);
    }

    function set(oController, sPath, vValue) {
        return ModelStateRuntime.write(oController, VIEW_MODEL, sPath, vValue);
    }

    function setMany(oController, mValues) {
        return ModelStateRuntime.setMany(oController, VIEW_MODEL, mValues);
    }

    function replace(oController, vState) {
        return initModel(oController, vState);
    }

    function setFlag(oController, sPath, vValue) {
        set(oController, sPath, vValue);
        return vValue;
    }

    function withFlag(oController, sPath, fnWork, vBegin, vEnd) {
        return ModelStateRuntime.withFlag(oController, VIEW_MODEL, sPath, fnWork, vBegin, vEnd);
    }

    return Object.freeze({
        initModel: initModel,
        get: get,
        set: set,
        setMany: setMany,
        replace: replace,
        setFlag: setFlag,
        withFlag: withFlag,
        viewState: function (oController) {
            return ControllerModelRuntime.viewState(oController);
        }
    });
});
