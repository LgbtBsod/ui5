sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast"
], function (BaseController, BackendAdapter, MessageToast) {
  "use strict";

  function _clone(vData) {
    return JSON.parse(JSON.stringify(vData));
  }

  return BaseController.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.attachRouteMatched("object", this._onMatched);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var oModel = this.getModel("data");
      var oState = this.getModel("state");
      var aChecklists = oModel.getProperty("/checkLists") || [];
      var oFoundChecklist = aChecklists.find(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      var oObjectData;
      var sAction = oState.getProperty("/objectAction") || "";
      var bCreate = sId === "__create" || sAction === "CREATE";
      var bCopy = sAction === "COPY";

      if (bCreate) {
        oObjectData = {
          root: { id: "" },
          basic: {
            date: "",
            timezone: "Europe/Amsterdam"
          },
          checks: [],
          barriers: []
        };
      } else if (oFoundChecklist) {
        oObjectData = _clone(oFoundChecklist);
      } else {
        oObjectData = {
          root: { id: sId },
          basic: {
            date: "",
            timezone: "Europe/Amsterdam"
          },
          checks: [],
          barriers: []
        };
      }


      if (bCopy && oObjectData && oObjectData.root) {
        oObjectData.root.id = "";
      }

      oState.setProperty("/mode", (bCreate || bCopy) ? "EDIT" : "READ");
      oState.setProperty("/objectAction", "");
      oModel.setProperty("/object", oObjectData);
      oModel.setProperty("/objectOriginal", _clone(oObjectData));

      this.getOwnerComponent().getRootControl().byId("fcl").setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      this.getModel("state").setProperty("/mode", isEdit ? "EDIT" : "READ");
    },

    onSave: function () {
      var oDataModel = this.getModel("data");
      var oStateModel = this.getModel("state");
      var oBundle = this.getResourceBundle();
      var oObject = _clone(oDataModel.getProperty("/object") || {});
      var sId = (((oObject || {}).root || {}).id || "").trim();
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var iExistingIndex = aChecklists.findIndex(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      var bCreateMode = !sId || iExistingIndex < 0;
      var pSave;

      oStateModel.setProperty("/isBusy", true);

      pSave = bCreateMode
        ? BackendAdapter.createCheckList(oObject)
        : BackendAdapter.updateCheckList(sId, oObject);

      pSave.then(function (oSavedChecklist) {
        if (!oSavedChecklist || !oSavedChecklist.root || !oSavedChecklist.root.id) {
          throw new Error(oBundle.getText("objectSaveInvalidResponse"));
        }

        return BackendAdapter.getCheckLists().then(function (aUpdatedCheckLists) {
          return {
            savedChecklist: oSavedChecklist,
            checkLists: aUpdatedCheckLists
          };
        });
      }).then(function (oResult) {
        var oSavedChecklist = oResult.savedChecklist;
        var aUpdatedCheckLists = oResult.checkLists;

        oDataModel.setProperty("/checkLists", aUpdatedCheckLists);
        oDataModel.setProperty("/visibleCheckLists", aUpdatedCheckLists);
        oDataModel.setProperty("/selectedChecklist", oSavedChecklist);
        oDataModel.setProperty("/object", _clone(oSavedChecklist));
        oDataModel.setProperty("/objectOriginal", _clone(oSavedChecklist));
        oStateModel.setProperty("/mode", "READ");

        MessageToast.show(oBundle.getText("objectSaved"));
        this.navTo("detail", { id: oSavedChecklist.root.id });
      }.bind(this)).catch(function (oError) {
        MessageToast.show(oBundle.getText("objectSaveFailed", [((oError && oError.message) || "Unknown error")]));
      }).finally(function () {
        oStateModel.setProperty("/isBusy", false);
      });
    },

    onCancel: function () {
      var oDataModel = this.getModel("data");
      var oOriginal = oDataModel.getProperty("/objectOriginal");
      var sOriginalId = ((((oOriginal || {}).root || {}).id) || "").trim();

      if (oOriginal) {
        oDataModel.setProperty("/object", _clone(oOriginal));
      }

      this.getModel("state").setProperty("/mode", "READ");

      if (sOriginalId) {
        this.navTo("detail", { id: sOriginalId }, true);
        return;
      }

      this.navTo("search", {}, true);
    }

  });
});
