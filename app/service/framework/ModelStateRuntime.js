sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailMessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (CloneUtil, StatePaths, WorkflowContracts, WorkflowRuntimeConstants, ModelPathContracts, JsRuntime, DetailMessageKeyConstants, ModelContracts) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;
    var METHODS = JsRuntime.METHODS;
    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;
    var MODELS = ModelContracts.MODELS;

    function model(oController, sModelName) {
        return oController && typeof oController[METHODS.GET_MODEL] === TYPE_FUNCTION ? oController[METHODS.GET_MODEL](sModelName) : null;
    }

    function writeOnModel(oModel, sPath, vValue) {
        if (!oModel || typeof oModel[METHODS.SET_PROPERTY] !== TYPE_FUNCTION) {
            return false;
        }
        oModel[METHODS.SET_PROPERTY](sPath, vValue);
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
        if (!oModel || typeof oModel[METHODS.GET_PROPERTY] !== TYPE_FUNCTION) {
            return vFallback;
        }
        vValue = oModel[METHODS.GET_PROPERTY](sPath);
        return typeof vValue === TYPE_UNDEFINED ? vFallback : vValue;
    }

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = model(oController, sModelName);
        return readOnModel(oModel, sPath, vFallback);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = model(oController, sModelName);
        if (!oModel || typeof oModel[METHODS.SET_PROPERTY] !== TYPE_FUNCTION) {
            return false;
        }
        oModel[METHODS.SET_PROPERTY](sPath, vValue);
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

    function clone(vValue, vFallback) {
        return CloneUtil.clone(vValue, vFallback);
    }

    function syncDetailCurrent(oController, vData) {
        return !!(oController && vData);
    }

    function resetDetailWorkflowState(oController, mPatch) {
        return setMany(oController, MODELS.STATE, Object.assign({
            [StatePaths.WORKFLOW_DETAIL_EDIT_MODE]: WorkflowContracts.EDIT_MODES.READ,
            [StatePaths.WORKFLOW_DETAIL_LOCK_STATE]: WorkflowContracts.LOCK_STATES.IDLE,
            "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.IDLE,
            "/autosaveAt": null,
            "/autosaveEnabled": false,
            "/isDirty": false,
            [ModelPathContracts.ACTIVE_OBJECT_ID]: "",
            [ModelPathContracts.SELECTED_ID]: "",
            "/persistence": {
                state: WorkflowRuntimeConstants.PERSISTENCE_STATES.IDLE,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_IDLE,
                lastSavedAt: null,
                lastSaveError: null,
                taxonomy: "",
                currentWriteRequestId: "",
                isManualSaveInFlight: false,
                isAutoSaveInFlight: false,
                hasValidLock: false,
                lockOwnerSessionMatches: false,
                lastLockRefreshAt: null,
                nextHeartbeatAt: null
            }
        }, mPatch || {}));
    }

    function resetDetailRuntimeData(oController) {
        replaceData(oController, MODELS.DETAIL, {
            current: {},
            base: {}
        });
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
        clone: clone,
        syncDetailCurrent: syncDetailCurrent,
        resetDetailWorkflowState: resetDetailWorkflowState,
        resetDetailRuntimeData: resetDetailRuntimeData,
        withFlag: withFlag,
        any: any,
        withFlags: withFlags
    };
});
