sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/EncodingUtils"
], function (LockAdapter, ModelContracts, ControllerModelRuntime, JsRuntime, EncodingUtils) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;
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
            var oI18nModel = oOwner && typeof oOwner.getModel === TYPE_FUNCTION ? oOwner.getModel(MODELS.I18N) : null;
            return oI18nModel && typeof oI18nModel.getResourceBundle === TYPE_FUNCTION ? oI18nModel.getResourceBundle() : null;
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
            return EncodingUtils.base64ToHex(sBase64);
        }
    };
});
