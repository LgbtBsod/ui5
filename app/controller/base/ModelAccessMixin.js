sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/LockFacade"], function (LockFacade) {
    "use strict";

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
        getResourceBundle: function () { return this.getOwnerComponent().getModel("i18n").getResourceBundle(); },
        releaseLock: function (sObjectId, sSessionId) {
            if (!sObjectId || !sSessionId) { return Promise.resolve(); }
            return LockFacade.release(sObjectId, sSessionId);
        },
        setLockUiState: function (oStateModel, sState, sText) {
            if (!oStateModel) { return; }
            oStateModel.setProperty("/lockOperationState", sState || "IDLE");
            oStateModel.setProperty("/lockOperationText", sText || "");
        },
        setLockPending: function (oStateModel, bPending) { if (oStateModel) { oStateModel.setProperty("/lockOperationPending", !!bPending); } },
        deleteRowFromEvent: function (oEvent, sModelName, sCollectionPath) {
            var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext(sModelName);
            if (!oCtx) { return { deleted: false }; }
            var iIndex = Number(oCtx.getPath().split("/").pop());
            var oModel = this.getModel(sModelName);
            var aItems = oModel.getProperty(sCollectionPath) || [];
            if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) { return { deleted: false }; }
            var aNext = aItems.slice();
            aNext.splice(iIndex, 1);
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
            var sBinary = atob(String(sBase64));
            var aHex = [];
            for (var i = 0; i < sBinary.length; i += 1) { var h = sBinary.charCodeAt(i).toString(16); aHex.push(h.length >= 2 ? h : "0" + h); }
            return aHex.join("");
        }
    };
});
