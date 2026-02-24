sap.ui.define([
  "sap_ui5/controller/Base"
], function (BaseController) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.attachRouteMatched("object", this._onMatched);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var oModel = this.getModel("data");
      var aChecklists = oModel.getProperty("/checkLists") || [];
      var oFoundChecklist = aChecklists.find(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      var oObjectData;

      if (sId === "__create") {
        oObjectData = {
          root: { id: "" },
          basic: {
            date: "",
            timezone: "Europe/Amsterdam"
          },
          checks: []
        };
      } else if (oFoundChecklist) {
        oObjectData = {
          root: {
            id: oFoundChecklist.root.id
          },
          basic: {
            date: oFoundChecklist.basic.date,
            timezone: oFoundChecklist.basic.timezone
          },
          checks: (oFoundChecklist.checks || []).map(function (oCheck, iIndex) {
            return {
              no: iIndex + 1,
              text: oCheck.text,
              result: Boolean(oCheck.result)
            };
          })
        };
      } else {
        oObjectData = {
          root: { id: sId },
          basic: {
            date: "",
            timezone: "Europe/Amsterdam"
          },
          checks: []
        };
      }

      oModel.setProperty("/object", oObjectData);

      this.getOwnerComponent().getRootControl().byId("fcl").setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      this.getModel("state").setProperty("/mode", isEdit ? "EDIT" : "READ");
    }

  });
});
