sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes"
], function (BehaviorScopes) {
    "use strict";

    function runOperation(sOperation, mContext) {
        return BehaviorScopes.workflow.execute(sOperation, mContext || {});
    }

    function runSyncOperation(sOperation, mContext) {
        return BehaviorScopes.workflow.executeSync(sOperation, mContext || {});
    }

    return {
        extractBackendDetail: function (oError) {
            return runSyncOperation("extractBackendDetail", {
                error: oError || null
            });
        },
        releaseActiveLock: function (oController, mPayload) {
            return runOperation("releaseActiveLock", {
                controller: oController,
                payload: mPayload || {}
            });
        },
        releaseWithTrySave: function (oController, mPayload) {
            return runOperation("releaseWithTrySave", {
                controller: oController,
                payload: mPayload || {}
            });
        },
        confirmUnsavedAndHandle: function (oController, fnOnSave, mOptions) {
            return runOperation("confirmUnsavedAndHandle", {
                controller: oController,
                onSave: fnOnSave,
                onCancel: mOptions && mOptions.onCancel
            });
        },
        handleBackendError: function (oController, oError, mHandlers) {
            return runOperation("handleBackendError", {
                controller: oController,
                error: oError || null,
                handlers: mHandlers || {}
            });
        },
        confirmStealOwnLock: function (oController) {
            return runOperation("confirmStealOwnLock", {
                controller: oController
            });
        },
        showLockKilledNotice: function (oController) {
            return runOperation("showLockKilledNotice", {
                controller: oController
            });
        },
        registerBehaviorOverride: BehaviorScopes.workflow.registerBehaviorOverride,
        unregisterBehaviorOverride: BehaviorScopes.workflow.unregisterBehaviorOverride,
        clearBehaviorOverrides: BehaviorScopes.workflow.clearBehaviorOverrides
    };
});
