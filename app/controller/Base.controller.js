sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/RouterMixin",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/EffectMixin",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ThemeMixin",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/EncodingUtils"
], function (Controller, RouterMixin, EffectMixin, ThemeMixin, LockAdapter, ModelContracts, JsRuntime, EncodingUtils) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;
    var MODELS = ModelContracts.MODELS;

    function resolveOwner(oController) {
        return oController && typeof oController.getOwnerComponent === TYPE_FUNCTION ? oController.getOwnerComponent() : null;
    }

    function resolveNamedModel(oController, sName) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION ? oController.getView() : null;
        var oOwner = resolveOwner(oController);
        return (oView && typeof oView.getModel === TYPE_FUNCTION && oView.getModel(sName))
            || (oOwner && typeof oOwner.getModel === TYPE_FUNCTION && oOwner.getModel(sName))
            || null;
    }

    return Controller.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Base", Object.assign({}, RouterMixin, EffectMixin, ThemeMixin, {
        getModel: function (sName) {
            if (typeof sName === TYPE_UNDEFINED) {
                return resolveNamedModel(this, undefined);
            }
            return resolveNamedModel(this, sName);
        },
        setModel: function (oModel, sName) {
            return this.getView().setModel(oModel, sName);
        },
        getResourceBundle: function () {
            var oOwner = resolveOwner(this);
            var oI18nModel = oOwner && typeof oOwner.getModel === TYPE_FUNCTION ? oOwner.getModel(MODELS.I18N) : null;
            return oI18nModel && typeof oI18nModel.getResourceBundle === TYPE_FUNCTION ? oI18nModel.getResourceBundle() : null;
        },
        releaseLock: function (sObjectId, sSessionId) {
            if (!sObjectId || !sSessionId) {
                return Promise.resolve();
            }
            return LockAdapter.release({ dbKey: sObjectId, sessionGuid: sSessionId }).catch(function () {
                return null;
            });
        },
        setLockPending: function (oStateModel, bPending) {
            if (oStateModel) {
                oStateModel.setProperty("/lockOperationPending", !!bPending);
            }
        },
        deleteRowFromEvent: function (oEvent, sModelName, sCollectionPath) {
            var oCtx = oEvent && typeof oEvent.getSource === TYPE_FUNCTION && oEvent.getSource().getBindingContext(sModelName);
            var oModel;
            var aItems;
            var oTargetObject;
            var iIndex;

            if (!oCtx) {
                return { deleted: false };
            }
            oModel = this.getModel(sModelName);
            aItems = oModel.getProperty(sCollectionPath) || [];
            oTargetObject = oCtx.getObject && oCtx.getObject();
            iIndex = aItems.indexOf(oTargetObject);
            if (iIndex < 0) {
                iIndex = Number(oCtx.getPath().split("/").pop());
            }
            if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
                return { deleted: false };
            }
            oModel.setProperty(sCollectionPath, aItems.filter(function (_oItem, iItemIndex) {
                return iItemIndex !== iIndex;
            }));
            return { deleted: true, index: iIndex };
        },
        runWithStateFlag: function (oStateModel, sPath, fnTask) {
            if (!oStateModel || !sPath || typeof fnTask !== TYPE_FUNCTION) {
                return Promise.resolve(null);
            }
            oStateModel.setProperty(sPath, true);
            return Promise.resolve().then(fnTask).finally(function () {
                oStateModel.setProperty(sPath, false);
            });
        },
        base64ToHex: function (sBase64) {
            return EncodingUtils.base64ToHex(sBase64);
        }
    }));
});
