sap.ui.define([], function () {
    "use strict";

    function runOpen(mArgs) {
        if (typeof mArgs.runOpenLifecycle !== "function") {
            return { ok: false, reason: "missing_open_lifecycle" };
        }

        return mArgs.runOpenLifecycle({
            applyDegradedState: function () {
                if (mArgs.isSmartControlsEnabled !== true && typeof mArgs.applyAnalyticsError === "function") {
                    mArgs.applyAnalyticsError(mArgs.smartControlsUnavailableText || "");
                }
            },
            openDialog: function () {
                if (typeof mArgs.openDialogLifecycle === "function") {
                    mArgs.openDialogLifecycle({
                        dialog: mArgs.dialog,
                        runLoad: mArgs.runLoad,
                        openDialog: mArgs.openDialog
                    });
                }
            }
        });
    }

    function runClose(mArgs) {
        if (typeof mArgs.runCloseLifecycle !== "function") {
            return { ok: false, reason: "missing_close_lifecycle" };
        }

        return mArgs.runCloseLifecycle({
            closeDialog: function () {
                if (typeof mArgs.closeDialogLifecycle === "function") {
                    mArgs.closeDialogLifecycle({
                        dialog: mArgs.dialog,
                        closeDialog: mArgs.closeDialog
                    });
                }
            }
        });
    }

    return {
        runOpen: runOpen,
        runClose: runClose
    };
});
