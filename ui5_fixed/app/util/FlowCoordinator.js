sap.ui.define([
  "checklist/app/infra/adapters/LockAdapter",
  "checklist/app/service/framework/DialogOrchestrator",
  "checklist/app/model/StatePaths",
  "checklist/app/service/framework/FeedbackCoordinator",
  "checklist/app/service/framework/RootIdRuntime",
  "checklist/app/service/framework/ModelStateRuntime"
], function (LockAdapter, DialogOrchestrator, StatePaths, FeedbackCoordinator, RootIdRuntime, ModelStateRuntime) {
  "use strict";

  function statusFromError(oError) {
    var m = String((oError && oError.message) || "").match(/HTTP\s+(\d+)/i);
    return m ? Number(m[1]) : 0;
  }

  function extractBackendDetail(oError) {
    var sMessage = String((oError && oError.message) || "");
    var iJsonStart = sMessage.indexOf("{");
    if (iJsonStart < 0) { return null; }
    try { return JSON.parse(sMessage.slice(iJsonStart)); } catch (e) { return null; }
  }

  function releaseWithTrySave(oHostController, mPayload) {
    return LockAdapter.create().release({
      rootId: RootIdRuntime.resolveFromStateModel(ModelStateRuntime.model(oHostController, "state")),
      sessionGuid: ModelStateRuntime.read(oHostController, "state", StatePaths.SESSION_ID, ""),
      payload: mPayload || {}
    }).catch(function () { return null; });
  }

  function resolveUnsavedAction(sAction, oHostController, fnOnSave) {
    if (sAction === DialogOrchestrator.actions.YES) {
      return Promise.resolve(fnOnSave && fnOnSave()).then(function (vSaveResult) {
        return (vSaveResult === false || (vSaveResult && vSaveResult.ok === false)) ? "SAVE_FAILED" : "SAVE";
      }).catch(function () {
        return "SAVE_FAILED";
      });
    }
    if (sAction === DialogOrchestrator.actions.NO) {
      return releaseWithTrySave(oHostController).then(function () {
        ModelStateRuntime.resetDetailWorkflowState(oHostController, {
          [StatePaths.WORKFLOW_LOCK_STATUS]: ModelStateRuntime.read(oHostController, "state", StatePaths.WORKFLOW_LOCK_STATUS, "IDLE") || "IDLE",
          [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE]: ModelStateRuntime.read(oHostController, "state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE") || "IDLE",
          [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT]: ModelStateRuntime.read(oHostController, "state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
          [StatePaths.WORKFLOW_AUTOSAVE_ENABLED]: ModelStateRuntime.read(oHostController, "state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false) === true,
          "/activeObjectId": ModelStateRuntime.read(oHostController, "state", "/activeObjectId", "") || ""
        });
        return "DISCARD";
      });
    }
    return Promise.resolve("CANCEL");
  }

  function confirmUnsavedAndHandle(oHostController, fnOnSave) {
    if (!ModelStateRuntime.read(oHostController, "state", "/isDirty", false)) { return Promise.resolve("NO_CHANGES"); }
    var sText = FeedbackCoordinator.resolveText(oHostController, "unsavedChangesPrompt", [], "unsavedChangesPrompt");
    var aActions = [DialogOrchestrator.actions.YES, DialogOrchestrator.actions.NO, DialogOrchestrator.actions.CANCEL];
    return DialogOrchestrator.promptConfirm(sText, aActions, DialogOrchestrator.actions.YES).then(function (sAction) { return resolveUnsavedAction(sAction, oHostController, fnOnSave); });
  }

  function handleBackendError(oHostController, oError, mHandlers) {
    var iStatus = statusFromError(oError);
    if (iStatus === 409) {
      var a = [
        FeedbackCoordinator.resolveText(oHostController, "reloadButton", [], "reloadButton"),
        FeedbackCoordinator.resolveText(oHostController, "overwriteButton", [], "overwriteButton"),
        DialogOrchestrator.actions.CANCEL
      ];
      return DialogOrchestrator.promptWarning(
        FeedbackCoordinator.resolveText(oHostController, "conflictDialogText", [], "conflictDialogText"),
        a
      ).then(function (sAction) { return mHandlers && mHandlers.onConflictChoice ? mHandlers.onConflictChoice(sAction) : sAction; });
    }
    if (iStatus === 410 && mHandlers && mHandlers.onLockExpired) { return mHandlers.onLockExpired(); }
    DialogOrchestrator.promptError(
      FeedbackCoordinator.resolveText(
        oHostController,
        "genericOperationFailed",
        [((oError && oError.message) || "Unknown error")],
        "genericOperationFailed"
      )
    );
    return Promise.resolve(null);
  }

  function confirmStealOwnLock(oHostController) {
    var sYes = FeedbackCoordinator.resolveText(oHostController, "yesButton", [], "yesButton");
    var sNo = FeedbackCoordinator.resolveText(oHostController, "noButton", [], "noButton");
    var a = [sYes, sNo];
    return DialogOrchestrator.promptWarning(
      FeedbackCoordinator.resolveText(oHostController, "lockStealOwnSessionPrompt", [], "lockStealOwnSessionPrompt"),
      a,
      sYes
    ).then(function (sAction) { return sAction === sYes; });
  }

  function showLockKilledNotice(oHostController) {
    DialogOrchestrator.promptWarning(
      FeedbackCoordinator.resolveText(oHostController, "lockKilledMessage", [], "lockKilledMessage"),
      [FeedbackCoordinator.resolveText(oHostController, "okButton", [], "okButton")]
    );
  }

  return { extractBackendDetail: extractBackendDetail, confirmStealOwnLock: confirmStealOwnLock, showLockKilledNotice: showLockKilledNotice, releaseWithTrySave: releaseWithTrySave, confirmUnsavedAndHandle: confirmUnsavedAndHandle, handleBackendError: handleBackendError };
});
