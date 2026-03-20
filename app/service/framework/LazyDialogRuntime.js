sap.ui.define([
    "sap/ui/core/Fragment"
], function (Fragment) {
    "use strict";

    function resolveCacheProperty(mConfig) {
        var sProp = mConfig && mConfig.cacheProperty;
        return typeof sProp === "string" && sProp ? sProp : "_mLazyDialogs";
    }

    function ensureMap(oController, mConfig) {
        var sProp = resolveCacheProperty(mConfig);
        if (!oController[sProp]) {
            oController[sProp] = {};
        }
        return oController[sProp];
    }

    function resolveDialogId(oController, sKey, mConfig) {
        if (mConfig && mConfig.dialogId) {
            return mConfig.dialogId;
        }
        return sKey + "Dialog";
    }

    function bindDialogHooks(oController, sKey, oDialog, mConfig) {
        var fnAfterOpen = mConfig && mConfig.afterOpen;
        var fnAfterClose = mConfig && mConfig.afterClose;

        if (oDialog && typeof oDialog.attachAfterOpen === "function" && typeof fnAfterOpen === "function") {
            oDialog.attachAfterOpen(function () {
                fnAfterOpen(oDialog, oController, sKey);
            });
        }
        if (oDialog && typeof oDialog.attachAfterClose === "function" && typeof fnAfterClose === "function") {
            oDialog.attachAfterClose(function () {
                fnAfterClose(oDialog, oController, sKey);
            });
        }
    }

    function ensureDialog(oController, sKey, mConfig) {
        var mCache = ensureMap(oController, mConfig);
        var sFragmentName = mConfig && mConfig.fragmentName;
        var sDialogId = resolveDialogId(oController, sKey, mConfig || {});

        if (mCache[sDialogId]) {
            return Promise.resolve(mCache[sDialogId]);
        }
        if (!sFragmentName) {
            return Promise.resolve(null);
        }

        return Fragment.load({
            id: oController.getView().getId(),
            name: sFragmentName,
            controller: oController
        }).then(function (oDialog) {
            mCache[sDialogId] = oDialog;
            oController.getView().addDependent(oDialog);
            bindDialogHooks(oController, sKey, oDialog, mConfig || {});
            return oDialog;
        });
    }

    function resolveDialog(oController, sKey, mConfig) {
        if (!oController || !sKey) {
            return null;
        }
        var sDialogId = resolveDialogId(oController, sKey, mConfig || {});
        return (oController.byId && oController.byId(sDialogId)) || null;
    }

    return {
        ensureDialog: ensureDialog,
        resolveDialog: resolveDialog
    };
});
