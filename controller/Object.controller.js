sap.ui.define([
  "sap/ui/core/mvc/Controller"
], function (Controller) {
  "use strict";

  return Controller.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.getOwnerComponent().getRouter()
        .getRoute("object")
        .attachPatternMatched(this._onMatched, this);
    },

    _onMatched: function (oEvent) {
      const sId = oEvent.getParameter("arguments").id;
      const oComponent = this.getOwnerComponent();
      const oModel = oComponent.getModel("data");
      const aChecklists = oModel.getProperty("/checkLists") || [];
      const oFoundChecklist = aChecklists.find(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      let oObjectData;

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

      // Switch to mid column
      const oFCL = oComponent.getRootControl().byId("fcl");
      oFCL.setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      this.getOwnerComponent().getModel("state").setProperty("/mode", isEdit ? "EDIT" : "READ");
    }

  });
});
