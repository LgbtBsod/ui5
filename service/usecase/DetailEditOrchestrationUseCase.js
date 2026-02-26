sap.ui.define([], function () {
    "use strict";

    function runToggleEditFlow(mArgs) {
        var bEditMode = !!mArgs.editMode;

        function runAcquireWithRecovery() {
            return mArgs.acquireLock().then(function (oAcquireResult) {
                if (!oAcquireResult) {
                    return null;
                }
                mArgs.onLockAcquired();
                return oAcquireResult;
            }).catch(function (oError) {
                return mArgs.tryRecoverFromAcquireError(oError).then(function (bRecovered) {
                    if (bRecovered) {
                        return null;
                    }
                    return mArgs.onAcquireFailed(oError);
                });
            });
        }

        if (mArgs.shouldPromptBeforeDisableEdit(bEditMode, mArgs.isDirty)) {
            return mArgs.confirmUnsaved().then(function (sDecision) {
                if (mArgs.isCancelDecision(sDecision)) {
                    return null;
                }
                return mArgs.runPendingRelease();
            });
        }

        return mArgs.runPendingToggle(function () {
            if (!bEditMode) {
                return mArgs.releaseEdit();
            }

            return mArgs.ensureFreshBeforeEdit().then(function () {
                return mArgs.confirmIntegrationEdit();
            }).then(function (bConfirmed) {
                if (!bConfirmed) {
                    mArgs.onStayReadOnly();
                    return null;
                }
                return runAcquireWithRecovery();
            }).catch(function (oError) {
                return mArgs.onAcquireFailed(oError);
            });
        });
    }

    return {
        runToggleEditFlow: runToggleEditFlow
    };
});
