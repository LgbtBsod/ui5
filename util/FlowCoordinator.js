sap.ui.define([
  "sap/m/MessageBox",
  "sap_ui5/service/backend/BackendAdapter"
], function (MessageBox, BackendAdapter) {
  "use strict";

  function _statusFromError(oError) {
    var sMessage = (oError && oError.message) || "";
    var m = sMessage.match(/HTTP\s+(\d+)/i);
    return m ? Number(m[1]) : 0;
  }

  function _showConflict(oHostController) {
    var oBundle = oHostController.getResourceBundle();
    return new Promise(function (resolve) {
      MessageBox.warning(oBundle.getText("conflictDialogText"), {
        actions: [oBundle.getText("reloadButton"), oBundle.getText("overwriteButton"), MessageBox.Action.CANCEL],
        onClose: function (sAction) {
          resolve(sAction);
        }
      });
    });
  }

  return {
    releaseWithTrySave: function (oHostController, mPayload) {
      var oState = oHostController.getModel("state");
      return BackendAdapter.lockRelease(
        oState.getProperty("/activeObjectId"),
        oState.getProperty("/sessionId"),
        { trySave: true, payload: mPayload || {} }
      ).catch(function () {
        return null;
      });
    },

    confirmUnsavedAndHandle: function (oHostController, fnOnSave) {
      var oState = oHostController.getModel("state");
      if (!oState.getProperty("/isDirty")) {
        return Promise.resolve("NO_CHANGES");
      }

      return new Promise(function (resolve) {
        MessageBox.confirm(oHostController.getResourceBundle().getText("unsavedChangesPrompt"), {
          actions: [MessageBox.Action.YES, MessageBox.Action.NO, MessageBox.Action.CANCEL],
          emphasizedAction: MessageBox.Action.YES,
          onClose: function (sAction) {
            if (sAction === MessageBox.Action.YES) {
              Promise.resolve(fnOnSave && fnOnSave()).finally(function () {
                resolve("SAVE");
              });
              return;
            }

            if (sAction === MessageBox.Action.NO) {
              this.releaseWithTrySave(oHostController).finally(function () {
                oState.setProperty("/isDirty", false);
                oState.setProperty("/mode", "READ");
                resolve("DISCARD");
              });
              return;
            }

            resolve("CANCEL");
          }.bind(this)
        });
      }.bind(this));
    },

    handleBackendError: function (oHostController, oError, mHandlers) {
      var iStatus = _statusFromError(oError);
      var oBundle = oHostController.getResourceBundle();

      if (iStatus === 409) {
        return _showConflict(oHostController).then(function (sAction) {
          if (mHandlers && mHandlers.onConflictChoice) {
            return mHandlers.onConflictChoice(sAction);
          }
          return sAction;
        });
      }

      if (iStatus === 410 && mHandlers && mHandlers.onLockExpired) {
        return mHandlers.onLockExpired();
      }

      MessageBox.error(oBundle.getText("genericOperationFailed", [((oError && oError.message) || "Unknown error")]));
      return Promise.resolve(null);
    }
  };
});
