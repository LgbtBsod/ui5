sap.ui.define([], function () {
    "use strict";

    function resolveModel(oController, sModelName) {
        return oController && oController.getModel ? oController.getModel(sModelName) : null;
    }

    function get(oController, sModelName, sPath, vFallback) {
        var oModel = resolveModel(oController, sModelName);
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        if (!sPath) {
            return vFallback;
        }
        return oModel.getProperty(sPath);
    }

    function set(oController, sModelName, sPath, vValue) {
        var oModel = resolveModel(oController, sModelName);
        if (!oModel || typeof oModel.setProperty !== "function" || !sPath) {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function setMany(oController, sModelName, mValues) {
        var mSource = mValues || {};
        var aKeys = Object.keys(mSource);
        var bChanged = false;
        aKeys.forEach(function (sPath) {
            bChanged = set(oController, sModelName, sPath, mSource[sPath]) || bChanged;
        });
        return bChanged;
    }

    function withFlag(oController, sModelName, sPath, fnWork, vBegin, vEnd) {
        set(oController, sModelName, sPath, typeof vBegin === "undefined" ? true : vBegin);
        return Promise.resolve()
            .then(function () {
                return typeof fnWork === "function" ? fnWork() : undefined;
            })
            .finally(function () {
                set(oController, sModelName, sPath, typeof vEnd === "undefined" ? false : vEnd);
            });
    }

    return {
        get: get,
        set: set,
        setMany: setMany,
        withFlag: withFlag
    };
});
