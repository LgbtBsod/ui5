sap.ui.define([
  "sap_ui5/facades/AppFacade"
], function (AppFacade) {
  "use strict";

  return {
    confirm: function (oHostController) {
      var oState = oHostController.getModel("state");
      var sLogin = (oState.getProperty("/testUserLogin") || "").trim();
      return AppFacade.confirmTestUser(oState, sLogin).then(function (oResult) {
        if (!oResult || !oResult.ok) {
          oHostController.showI18nToast("testUserEmpty");
          return false;
        }
        oHostController.showI18nToast("testUserApplied", [oResult.user]);
        return true;
      }).catch(function (oError) {
        oHostController.showI18nToast("testUserApplyFailed", [oError && oError.message || "Unknown error"]);
        return false;
      });
    }
  };
});
