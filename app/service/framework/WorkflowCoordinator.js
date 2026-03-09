sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowOverrideHandlers"
], function (BehaviorResolver, WorkflowDefaultHandlers, WorkflowOverrideHandlers) {
    "use strict";

    function runOperation(sOperation, mContext) {
        WorkflowDefaultHandlers.ensureRegistered();
        WorkflowOverrideHandlers.ensureRegistered();
        return BehaviorResolver.execute("workflow", sOperation, mContext || {}, WorkflowDefaultHandlers.handlers);
    }

    function runSyncOperation(sOperation, mContext) {
        WorkflowDefaultHandlers.ensureRegistered();
        WorkflowOverrideHandlers.ensureRegistered();
        return BehaviorResolver.executeSync("workflow", sOperation, mContext || {}, WorkflowDefaultHandlers.handlers);
    }

    return {
        extractBackendDetail: function (oError) {
            return runSyncOperation("extractBackendDetail", {
                error: oError || null
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
        registerBehaviorOverride: WorkflowOverrideHandlers.register,
        unregisterBehaviorOverride: WorkflowOverrideHandlers.unregister,
        clearBehaviorOverrides: WorkflowOverrideHandlers.clear
    };
});
