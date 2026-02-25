sap.ui.define([
    "sap/ui/core/Fragment"
], function (Fragment) {
    "use strict";

    function ensureDialog(mArgs) {
        var sProp = mArgs.prop;
        if (!mArgs.host[sProp]) {
            mArgs.host[sProp] = Fragment.load({
                id: mArgs.view.getId(),
                name: mArgs.fragment,
                controller: mArgs.controller
            }).then(function (oDialog) {
                mArgs.view.addDependent(oDialog);
                return oDialog;
            });
        }
        return mArgs.host[sProp];
    }

    function openDialog(mArgs) {
        return ensureDialog(mArgs).then(function (oDialog) {
            oDialog.open();
            return oDialog;
        });
    }

    function destroyDialogsByIds(mArgs) {
        (mArgs.ids || []).forEach(function (sId) {
            var oDialog = mArgs.byId(sId);
            if (oDialog) {
                oDialog.destroy();
            }
        });
        (mArgs.props || []).forEach(function (sProp) {
            mArgs.host[sProp] = null;
        });
    }

    return {
        openDialog: openDialog,
        destroyDialogsByIds: destroyDialogsByIds
    };
});
