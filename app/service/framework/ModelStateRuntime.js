sap.ui.define([], function () {
    "use strict";

    function model(oController, sModelName) {
        return oController && oController.getModel ? oController.getModel(sModelName) : null;
    }

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        return oModel.getProperty(sPath);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function replaceData(oController, sModelName, vData) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.setData !== "function") {
            return false;
        }
        oModel.setData(vData || {});
        return true;
    }

    function clone(vValue, vFallback) {
        try {
            return JSON.parse(JSON.stringify(typeof vValue === "undefined" ? vFallback : vValue));
        } catch (_e) {
            return typeof vFallback === "undefined" ? null : vFallback;
        }
    }

    function syncDetailCurrent(oController, vData) {
        return write(oController, "uiState", "/_detailCurrent", clone(vData || {}, {}));
    }

    function resetDetailRuntimeData(oController) {
        replaceData(oController, "selected", {});
        write(oController, "uiState", "/_detailSnapshot", {});
        write(oController, "uiState", "/_detailCurrent", {});
    }

    function withFlag(oController, sModelName, sPath, fnWork, vStart, vEnd) {
        write(oController, sModelName, sPath, typeof vStart === "undefined" ? true : vStart);
        return Promise.resolve().then(fnWork).finally(function () {
            write(oController, sModelName, sPath, typeof vEnd === "undefined" ? false : vEnd);
        });
    }

    function any(oController, sModelName, aPaths) {
        return (aPaths || []).some(function (sPath) {
            return !!read(oController, sModelName, sPath, false);
        });
    }

    function withFlags(oController, sModelName, aPaths, fnWork, vStart, vEnd) {
        var aFlagPaths = Array.isArray(aPaths) ? aPaths.slice() : [];
        var vStartValue = typeof vStart === "undefined" ? true : vStart;
        var vEndValue = typeof vEnd === "undefined" ? false : vEnd;
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
        read: read,
        write: write,
        replaceData: replaceData,
        clone: clone,
        syncDetailCurrent: syncDetailCurrent,
        resetDetailRuntimeData: resetDetailRuntimeData,
        withFlag: withFlag,
        any: any,
        withFlags: withFlags
    };
});
