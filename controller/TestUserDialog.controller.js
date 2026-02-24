sap.ui.define([
  "sap/m/MessageToast",
  "sap_ui5/service/backend/BackendAdapter"
], function (MessageToast, BackendAdapter) {
  "use strict";

  return {
    /**
     * Dedicated modal controller for test-user bootstrap (outside SAP runtime).
     */
    confirm: function (oHostController) {
      var oState = oHostController.getModel("state");
      var sLogin = (oState.getProperty("/testUserLogin") || "").trim();
      var oBundle = oHostController.getResourceBundle();

      if (!sLogin) {
        MessageToast.show(oBundle.getText("testUserEmpty"));
        return Promise.resolve(false);
      }

      return BackendAdapter.login(sLogin).then(function (oLoginResult) {
        window.sessionStorage.setItem("pcct_test_user", sLogin);
        oState.setProperty("/testUser", sLogin);
        oState.setProperty("/testUserLogin", sLogin);
        oState.setProperty("/requiresUserLogin", false);
        if (oLoginResult && oLoginResult.sessionId) {
          oState.setProperty("/sessionId", oLoginResult.sessionId);
        }
        MessageToast.show(oBundle.getText("testUserApplied", [sLogin]));
        return true;
      }).catch(function (oError) {
        MessageToast.show(oBundle.getText("testUserApplyFailed", [oError && oError.message || "Unknown error"]));
        return false;
      });
    }
  };
});
