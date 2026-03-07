sap.ui.define([
  "sap_ui5/infra/adapters/LockAdapter",
  "sap_ui5/service/framework/EffectApplier",
  "sap_ui5/model/StatePaths"
], function (LockAdapter, EffectApplier, StatePaths) {
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
    var oState = oHostController.getModel("state");
    return LockAdapter.create().release({ rootId: oState.getProperty("/activeObjectId"), sessionGuid: oState.getProperty(StatePaths.SESSION_ID), payload: mPayload || {} }).catch(function () { return null; });
  }

  function resolveUnsavedAction(sAction, oHostController, fnOnSave) {
    var oState = oHostController.getModel("state");
    if (sAction === EffectApplier.actions.YES) {
      return Promise.resolve(fnOnSave && fnOnSave()).then(function (vSaveResult) {
        return (vSaveResult === false || (vSaveResult && vSaveResult.ok === false)) ? "SAVE_FAILED" : "SAVE";
      }).catch(function () {
        return "SAVE_FAILED";
      });
    }
    if (sAction === EffectApplier.actions.NO) {
      return releaseWithTrySave(oHostController).then(function () {
        oState.setProperty("/isDirty", false);
        oState.setProperty("/mode", "READ");
        return "DISCARD";
      });
    }
    return Promise.resolve("CANCEL");
  }

  function confirmUnsavedAndHandle(oHostController, fnOnSave) {
    var oState = oHostController.getModel("state");
    if (!oState.getProperty("/isDirty")) { return Promise.resolve("NO_CHANGES"); }
    var sText = oHostController.getResourceBundle().getText("unsavedChangesPrompt");
    var aActions = [EffectApplier.actions.YES, EffectApplier.actions.NO, EffectApplier.actions.CANCEL];
    return EffectApplier.promptConfirm(sText, aActions, EffectApplier.actions.YES).then(function (sAction) { return resolveUnsavedAction(sAction, oHostController, fnOnSave); });
  }

  function handleBackendError(oHostController, oError, mHandlers) {
    var iStatus = statusFromError(oError);
    var oBundle = oHostController.getResourceBundle();
    if (iStatus === 409) {
      var a = [oBundle.getText("reloadButton"), oBundle.getText("overwriteButton"), EffectApplier.actions.CANCEL];
      return EffectApplier.promptWarning(oBundle.getText("conflictDialogText"), a).then(function (sAction) { return mHandlers && mHandlers.onConflictChoice ? mHandlers.onConflictChoice(sAction) : sAction; });
    }
    if (iStatus === 410 && mHandlers && mHandlers.onLockExpired) { return mHandlers.onLockExpired(); }
    EffectApplier.promptError(oBundle.getText("genericOperationFailed", [((oError && oError.message) || "Unknown error")]));
    return Promise.resolve(null);
  }

  function confirmStealOwnLock(oHostController) {
    var oBundle = oHostController.getResourceBundle();
    var a = [oBundle.getText("yesButton"), oBundle.getText("noButton")];
    return EffectApplier.promptWarning(oBundle.getText("lockStealOwnSessionPrompt"), a, oBundle.getText("yesButton")).then(function (sAction) { return sAction === oBundle.getText("yesButton"); });
  }

  function showLockKilledNotice(oHostController) {
    var oBundle = oHostController.getResourceBundle();
    EffectApplier.promptWarning(oBundle.getText("lockKilledMessage"), [oBundle.getText("okButton")]);
  }

  return { extractBackendDetail: extractBackendDetail, confirmStealOwnLock: confirmStealOwnLock, showLockKilledNotice: showLockKilledNotice, releaseWithTrySave: releaseWithTrySave, confirmUnsavedAndHandle: confirmUnsavedAndHandle, handleBackendError: handleBackendError };
});
