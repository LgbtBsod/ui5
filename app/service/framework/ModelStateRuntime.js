sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;

    function model(oController, sModelName) {
        return oController && typeof oController.getModel === TYPE_FUNCTION ? oController.getModel(sModelName) : null;
    }

    function writeOnModel(oModel, sPath, vValue) {
        if (!oModel || typeof oModel.setProperty !== TYPE_FUNCTION) {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function setManyOnModel(oModel, mValues) {
        var bWritten = false;
        Object.keys(mValues || {}).forEach(function (sPath) {
            bWritten = writeOnModel(oModel, sPath, mValues[sPath]) || bWritten;
        });
        return bWritten;
    }

    function readOnModel(oModel, sPath, vFallback) {
        var vValue;
        if (!oModel || typeof oModel.getProperty !== TYPE_FUNCTION) {
            return vFallback;
        }
        vValue = oModel.getProperty(sPath);
        return typeof vValue === TYPE_UNDEFINED ? vFallback : vValue;
    }

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = model(oController, sModelName);
        return readOnModel(oModel, sPath, vFallback);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.setProperty !== TYPE_FUNCTION) {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function writeBoolean(oController, sModelName, sPath, bValue) {
        var bNormalized = !!bValue;
        write(oController, sModelName, sPath, bNormalized);
        return bNormalized;
    }

    function setMany(oController, sModelName, mValues) {
        var bWritten = false;
        Object.keys(mValues || {}).forEach(function (sPath) {
            bWritten = write(oController, sModelName, sPath, mValues[sPath]) || bWritten;
        });
        return bWritten;
    }

    function replaceData(oController, sModelName, vData) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.setData !== TYPE_FUNCTION) {
            return false;
        }
        oModel.setData(vData || {});
        return true;
    }

    function syncDetailCurrent(oController, vData) {
        return !!(oController && vData);
    }

    function withFlag(oController, sModelName, sPath, fnWork, vStart, vEnd) {
        write(oController, sModelName, sPath, typeof vStart === TYPE_UNDEFINED ? true : vStart);
        return Promise.resolve().then(fnWork).finally(function () {
            write(oController, sModelName, sPath, typeof vEnd === TYPE_UNDEFINED ? false : vEnd);
        });
    }

    function any(oController, sModelName, aPaths) {
        return (aPaths || []).some(function (sPath) {
            return !!read(oController, sModelName, sPath, false);
        });
    }

    function withFlags(oController, sModelName, aPaths, fnWork, vStart, vEnd) {
        var aFlagPaths = Array.isArray(aPaths) ? aPaths.slice() : [];
        var vStartValue = typeof vStart === TYPE_UNDEFINED ? true : vStart;
        var vEndValue = typeof vEnd === TYPE_UNDEFINED ? false : vEnd;
        aFlagPaths.forEach(function (sPath) {
            write(oController, sModelName, sPath, vStartValue);
        });
        return Promise.resolve().then(fnWork).finally(function () {
            aFlagPaths.forEach(function (sPath) {
                write(oController, sModelName, sPath, vEndValue);
            });
        });
    }

    return {
        model: model,
        writeOnModel: writeOnModel,
        setManyOnModel: setManyOnModel,
        readOnModel: readOnModel,
        read: read,
        write: write,
        writeBoolean: writeBoolean,
        setMany: setMany,
        replaceData: replaceData,
        syncDetailCurrent: syncDetailCurrent,
        withFlag: withFlag,
        any: any,
        withFlags: withFlags
    };
});
