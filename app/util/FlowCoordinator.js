sap.ui.define([
  "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator"
], function (WorkflowCoordinator) {
  "use strict";

  return {
    extractBackendDetail: WorkflowCoordinator.extractBackendDetail,
    confirmStealOwnLock: WorkflowCoordinator.confirmStealOwnLock,
    showLockKilledNotice: WorkflowCoordinator.showLockKilledNotice,
    releaseWithTrySave: WorkflowCoordinator.releaseWithTrySave,
    confirmUnsavedAndHandle: WorkflowCoordinator.confirmUnsavedAndHandle,
    handleBackendError: WorkflowCoordinator.handleBackendError,
    registerBehaviorOverride: WorkflowCoordinator.registerBehaviorOverride,
    unregisterBehaviorOverride: WorkflowCoordinator.unregisterBehaviorOverride,
    clearBehaviorOverrides: WorkflowCoordinator.clearBehaviorOverrides
  };
});
