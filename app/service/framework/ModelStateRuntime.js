sap.ui.define([], function () {
    "use strict";

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        return oModel.getProperty(sPath);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function withFlag(oController, sModelName, sPath, fnWork, vStart, vEnd) {
        write(oController, sModelName, sPath, typeof vStart === "undefined" ? true : vStart);
        return Promise.resolve().then(fnWork).finally(function () {
            write(oController, sModelName, sPath, typeof vEnd === "undefined" ? false : vEnd);
        });
    }

    return {
        read: read,
        write: write,
        withFlag: withFlag
    };
});
