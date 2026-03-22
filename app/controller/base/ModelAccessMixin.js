sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (LockAdapter, ModelContracts, ControllerModelRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;
    var METHODS = JsRuntime.METHODS;
    var MODELS = ModelContracts.MODELS;

    function resolveNamedModel(oController, sName) {
        switch (sName) {
        case MODELS.STATE:
            return ControllerModelRuntime.state(oController);
        case MODELS.SHELL:
            return ControllerModelRuntime.shell(oController);
        case MODELS.DETAIL:
            return ControllerModelRuntime.detail(oController);
        case MODELS.MASTER_DATA:
            return ControllerModelRuntime.masterData(oController);
        case MODELS.VIEW:
            return ControllerModelRuntime.viewState(oController);
        case MODELS.I18N:
        case MODELS.DEVICE:
        case "mainService":
            return ControllerModelRuntime.model(oController, sName, true);
        default:
            return null;
        }
    }

    function decodeBase64(sBase64) {
        if (typeof window !== TYPE_UNDEFINED && typeof window.atob === TYPE_FUNCTION) {
            return window.atob(sBase64);
        }
        if (typeof atob === TYPE_FUNCTION) {
            return atob(sBase64);
        }
        throw new Error("base64_decode_unavailable");
    }

    return {
        getModel: function (sName) {
            if (typeof sName === TYPE_UNDEFINED) {
                return ControllerModelRuntime.defaultModel(this, true);
            }
            return resolveNamedModel(this, sName);
        },
        setModel: function (oModel, sName) { return this.getView().setModel(oModel, sName); },
        getResourceBundle: function () {
            var oOwner = typeof this.getOwnerComponent === TYPE_FUNCTION && this.getOwnerComponent();
            var oI18nModel = oOwner && typeof oOwner[METHODS.GET_MODEL] === TYPE_FUNCTION ? oOwner[METHODS.GET_MODEL](MODELS.I18N) : null;
            return oI18nModel && typeof oI18nModel[METHODS.GET_RESOURCE_BUNDLE] === TYPE_FUNCTION ? oI18nModel[METHODS.GET_RESOURCE_BUNDLE]() : null;
        },
        releaseLock: function (sObjectId, sSessionId) {
            if (!sObjectId || !sSessionId) { return Promise.resolve(); }
            return LockAdapter.release({ rootId: sObjectId, sessionGuid: sSessionId }).catch(function () { return null; });
        },
        setLockPending: function (oStateModel, bPending) { if (oStateModel) { oStateModel.setProperty("/lockOperationPending", !!bPending); } },
        deleteRowFromEvent: function (oEvent, sModelName, sCollectionPath) {
            var oCtx = oEvent && typeof oEvent.getSource === TYPE_FUNCTION && oEvent.getSource().getBindingContext(sModelName);
            if (!oCtx) { return { deleted: false }; }
            var oModel = this.getModel(sModelName);
            var aItems = oModel.getProperty(sCollectionPath) || [];
            var oTargetObject = oCtx.getObject && oCtx.getObject();
            var iIndex = aItems.indexOf(oTargetObject);
            if (iIndex < 0) {
                iIndex = Number(oCtx.getPath().split("/").pop());
            }
            if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) { return { deleted: false }; }
            var aNext = aItems.filter(function (_oItem, iItemIndex) {
                return iItemIndex !== iIndex;
            });
            oModel.setProperty(sCollectionPath, aNext);
            return { deleted: true, index: iIndex };
        },
        runWithStateFlag: function (oStateModel, sPath, fnTask) {
            if (!oStateModel || !sPath || typeof fnTask !== TYPE_FUNCTION) { return Promise.resolve(null); }
            oStateModel.setProperty(sPath, true);
            return Promise.resolve().then(fnTask).finally(function () { oStateModel.setProperty(sPath, false); });
        },
        base64ToHex: function (sBase64) {
            if (!sBase64) { return ""; }
            var sBinary = decodeBase64(String(sBase64));
            var aHex = [];
            for (var i = 0; i < sBinary.length; i += 1) { var h = sBinary.charCodeAt(i).toString(16); aHex.push(h.length >= 2 ? h : "0" + h); }
            return aHex.join("");
        }
    };
});
