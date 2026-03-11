sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (CloneUtil, StatePaths) {
    "use strict";

    function normalizeLegacyLockState(sCanonicalLockState) {
        switch (String(sCanonicalLockState || "").toUpperCase()) {
        case "EDIT_LOCKED":
            return "LOCKED";
        case "ACQUIRING_LOCK":
            return "ACQUIRING_LOCK";
        case "LOCK_LOST":
            return "LOCK_LOST";
        case "IDLE_TIMEOUT_GRACE":
            return "IDLE_TIMEOUT_GRACE";
        case "FORCED_READ_ONLY":
            return "FORCED_READ_ONLY";
        case "READ_ONLY":
        case "IDLE":
        default:
            return "IDLE";
        }
    }

    function syncLegacyWorkflowAliases(oModel, sPath) {
        var sCanonicalMode;
        var sCanonicalLockState;
        var sLegacyMode;
        var sLegacyLockState;
        if (!oModel || typeof oModel.setProperty !== "function") {
            return;
        }
        if (sPath !== StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sPath !== StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
            return;
        }
        sCanonicalMode = String(oModel.getProperty(StatePaths.WORKFLOW_DETAIL_EDIT_MODE) || "READ").toUpperCase();
        sCanonicalLockState = String(oModel.getProperty(StatePaths.WORKFLOW_DETAIL_LOCK_STATE) || "READ_ONLY").toUpperCase();
        sLegacyMode = sCanonicalMode === "CREATE"
            ? "CREATE"
            : (sCanonicalMode === "EDIT" && sCanonicalLockState === "EDIT_LOCKED" ? "EDIT" : "READ");
        sLegacyLockState = normalizeLegacyLockState(sCanonicalLockState);
        if (oModel.getProperty(StatePaths.WORKFLOW_EDIT_MODE) !== sLegacyMode) {
            oModel.setProperty(StatePaths.WORKFLOW_EDIT_MODE, sLegacyMode);
        }
        if (oModel.getProperty(StatePaths.WORKFLOW_LOCK_STATUS) !== sLegacyLockState) {
            oModel.setProperty(StatePaths.WORKFLOW_LOCK_STATUS, sLegacyLockState);
        }
    }

    function model(oController, sModelName) {
        return oController && oController.getModel ? oController.getModel(sModelName) : null;
    }

    function writeOnModel(oModel, sPath, vValue) {
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        syncLegacyWorkflowAliases(oModel, sPath);
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
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        vValue = oModel.getProperty(sPath);
        return typeof vValue === "undefined" ? vFallback : vValue;
    }

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = model(oController, sModelName);
        return readOnModel(oModel, sPath, vFallback);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        syncLegacyWorkflowAliases(oModel, sPath);
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
        if (!oModel || typeof oModel.setData !== "function") {
            return false;
        }
        oModel.setData(vData || {});
        return true;
    }

    function clone(vValue, vFallback) {
        return CloneUtil.clone(vValue, vFallback);
    }

    function syncDetailCurrent(oController, vData) {
        return !!(oController && vData);
    }

    function resetDetailWorkflowState(oController, mPatch) {
        return setMany(oController, "state", Object.assign({
            "/mode": "READ",
            "/lockOperationState": "IDLE",
            [StatePaths.WORKFLOW_DETAIL_EDIT_MODE]: "READ",
            [StatePaths.WORKFLOW_DETAIL_LOCK_STATE]: "IDLE",
            "/autosaveState": "IDLE",
            "/autosaveAt": null,
            "/autosaveEnabled": false,
            "/isDirty": false,
            "/activeObjectId": ""
        }, mPatch || {}));
    }

    function resetDetailRuntimeData(oController) {
        replaceData(oController, "selected", {});
        replaceData(oController, "snapshot", {});
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
        writeOnModel: writeOnModel,
        setManyOnModel: setManyOnModel,
        readOnModel: readOnModel,
        read: read,
        write: write,
        writeBoolean: writeBoolean,
        setMany: setMany,
        replaceData: replaceData,
        clone: clone,
        syncDetailCurrent: syncDetailCurrent,
        resetDetailWorkflowState: resetDetailWorkflowState,
        resetDetailRuntimeData: resetDetailRuntimeData,
        withFlag: withFlag,
        any: any,
        withFlags: withFlags
    };
});
