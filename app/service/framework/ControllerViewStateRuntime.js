sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "checklist/app/controller/support/ControllerModelWriteSupport"
], function (JSONModel, ControllerModelWriteSupport) {
    "use strict";

    function initModel(oController, vState) {
        var vData = typeof vState === "function" ? vState() : vState;
        oController.setModel(new JSONModel(vData || {}), "view");
        return oController.getModel("view");
    }

    function get(oController, sPath, vFallback) {
        return ControllerModelWriteSupport.get(oController, "view", sPath, vFallback);
    }

    function set(oController, sPath, vValue) {
        return ControllerModelWriteSupport.set(oController, "view", sPath, vValue);
    }

    function setMany(oController, mValues) {
        return ControllerModelWriteSupport.setMany(oController, "view", mValues);
    }

    function replace(oController, vState) {
        var oModel = oController && oController.getModel ? oController.getModel("view") : null;
        var vData = typeof vState === "function" ? vState() : vState;

        if (!oModel || typeof oModel.setData !== "function") {
            return initModel(oController, vData);
        }
        oModel.setData(vData || {});
        return oModel;
    }

    function setFlag(oController, sPath, vValue) {
        set(oController, sPath, vValue);
        return vValue;
    }

    function withFlag(oController, sPath, fnWork, vBegin, vEnd) {
        return ControllerModelWriteSupport.withFlag(oController, "view", sPath, fnWork, vBegin, vEnd);
    }

    return {
        initModel: initModel,
        get: get,
        set: set,
        setMany: setMany,
        replace: replace,
        setFlag: setFlag,
        withFlag: withFlag
    };
});
