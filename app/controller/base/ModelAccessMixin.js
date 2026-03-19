sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/LockFacade"
], function (LockFacade) {
    "use strict";

    function decodeBase64(sBase64) {
        if (typeof window !== "undefined" && typeof window.atob === "function") {
            return window.atob(sBase64);
        }
        if (typeof atob === "function") {
            return atob(sBase64);
        }
        throw new Error("base64_decode_unavailable");
    }

    return {
        getModel: function (sName) {
            var oView = this.getView && this.getView();
            var oViewModel = oView && oView.getModel ? oView.getModel(sName) : undefined;
            if (oViewModel) {
                return oViewModel;
            }
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            return oOwner && oOwner.getModel ? oOwner.getModel(sName) : undefined;
        },
        setModel: function (oModel, sName) { return this.getView().setModel(oModel, sName); },
        getResourceBundle: function () {
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            var oI18nModel = oOwner && oOwner.getModel ? oOwner.getModel("i18n") : null;
            return oI18nModel && typeof oI18nModel.getResourceBundle === "function" ? oI18nModel.getResourceBundle() : null;
        },
        releaseLock: function (sObjectId, sSessionId) {
            if (!sObjectId || !sSessionId) { return Promise.resolve(); }
            return LockFacade.release(sObjectId, sSessionId);
        },
        setLockPending: function (oStateModel, bPending) { if (oStateModel) { oStateModel.setProperty("/lockOperationPending", !!bPending); } },
        deleteRowFromEvent: function (oEvent, sModelName, sCollectionPath) {
            var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext(sModelName);
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
            if (!oStateModel || !sPath || typeof fnTask !== "function") { return Promise.resolve(null); }
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
