sap.ui.define([], function () {
    "use strict";

    function isDate(vValue) {
        return Object.prototype.toString.call(vValue) === "[object Date]";
    }

    function isPlainObject(vValue) {
        return !!vValue && Object.prototype.toString.call(vValue) === "[object Object]";
    }

    function cloneValue(vValue) {
        var oResult;

        if (Array.isArray(vValue)) {
            return vValue.map(cloneValue);
        }
        if (isDate(vValue)) {
            return new Date(vValue.getTime());
        }
        if (!isPlainObject(vValue)) {
            return vValue;
        }

        oResult = {};
        Object.keys(vValue).forEach(function (sKey) {
            oResult[sKey] = cloneValue(vValue[sKey]);
        });
        return oResult;
    }

    function getModel(oController, sModelName) {
        if (!oController || !oController.getModel) { return null; }
        return oController.getModel(sModelName);
    }

    function busy(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var sPath = oEffect.path || (oEffect.scope ? "/busy/" + oEffect.scope : "/busy");
        if (oModel && oModel.setProperty) { oModel.setProperty(sPath, !!oEffect.value); }
    }

    function patch(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        if (oModel && oModel.setProperty) { oModel.setProperty(oEffect.path, cloneValue(oEffect.value)); }
    }

    function merge(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var oCurrent;
        if (!oModel || !oModel.getProperty || !oModel.setProperty) { return; }
        oCurrent = oModel.getProperty(oEffect.path) || {};
        oModel.setProperty(oEffect.path, Object.assign({}, cloneValue(oCurrent), cloneValue(oEffect.partialObject || {})));
    }

    return { busy: busy, patch: patch, merge: merge };
});
