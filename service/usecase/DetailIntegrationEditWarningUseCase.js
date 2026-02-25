sap.ui.define([], function () {
    "use strict";

    function resolveRoot(mArgs) {
        return (mArgs && mArgs.selectedRoot)
            || (mArgs && mArgs.dataRoot)
            || {};
    }

    function isIntegrationRoot(oRoot) {
        return !!(oRoot && (oRoot.this_is_integration_data || oRoot.integrationFlag));
    }

    function shouldPrompt(mArgs) {
        return isIntegrationRoot(resolveRoot(mArgs));
    }

    function confirmIntegrationEdit(mArgs) {
        var oMessageBox = mArgs && mArgs.messageBox;
        var oBundle = mArgs && mArgs.bundle;

        if (!shouldPrompt(mArgs)) {
            return Promise.resolve(true);
        }
        if (!oMessageBox || typeof oMessageBox.warning !== "function") {
            return Promise.resolve(false);
        }

        return new Promise(function (resolve) {
            oMessageBox.warning(oBundle.getText("integrationEditWarning"), {
                title: oBundle.getText("integrationEditTitle"),
                actions: [oMessageBox.Action.YES, oMessageBox.Action.NO],
                emphasizedAction: oMessageBox.Action.NO,
                onClose: function (sAction) {
                    resolve(sAction === oMessageBox.Action.YES);
                }
            });
        });
    }

    return {
        resolveRoot: resolveRoot,
        isIntegrationRoot: isIntegrationRoot,
        shouldPrompt: shouldPrompt,
        confirmIntegrationEdit: confirmIntegrationEdit
    };
});
