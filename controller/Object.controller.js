sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/ui/model/json/JSONModel"
], function (BaseController, BackendAdapter, MessageToast, JSONModel) {
  "use strict";

  function _clone(vData) {
    return JSON.parse(JSON.stringify(vData));
  }

  return BaseController.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.setModel(new JSONModel({
        hasSelectedChecks: false,
        hasSelectedBarriers: false
      }), "view");
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
      var bEdit = sAction === "EDIT";

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

      oState.setProperty("/mode", (bCreate || bCopy || bEdit) ? "EDIT" : "READ");
      oState.setProperty("/objectAction", "");
      oModel.setProperty("/object", oObjectData);
      oModel.setProperty("/objectOriginal", _clone(oObjectData));
      this._updateSelectionState();

      this.getOwnerComponent().getRootControl().byId("fcl").setLayout("TwoColumnsMidExpanded");
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      this.getModel("state").setProperty("/mode", isEdit ? "EDIT" : "READ");
    },


    onAddCheckRow: function () {
      var oDataModel = this.getModel("data");
      var aChecks = oDataModel.getProperty("/object/checks") || [];

      aChecks.push({
        no: aChecks.length + 1,
        text: "",
        result: false,
        selected: false
      });

      oDataModel.setProperty("/object/checks", aChecks);
      this._updateSelectionState();
    },

    onAddBarrierRow: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];

      aBarriers.push({
        no: aBarriers.length + 1,
        text: "",
        result: false,
        selected: false
      });

      oDataModel.setProperty("/object/barriers", aBarriers);
      this._updateSelectionState();
    },


    _updateSelectionState: function () {
      var oDataModel = this.getModel("data");
      var aChecks = oDataModel.getProperty("/object/checks") || [];
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];
      this.getModel("view").setProperty("/hasSelectedChecks", aChecks.some(function (oItem) { return !!(oItem && oItem.selected); }));
      this.getModel("view").setProperty("/hasSelectedBarriers", aBarriers.some(function (oItem) { return !!(oItem && oItem.selected); }));
    },

    onSelectionToggle: function () {
      this._updateSelectionState();
    },

    onDeleteSelectedChecks: function () {
      var oDataModel = this.getModel("data");
      var aChecks = (oDataModel.getProperty("/object/checks") || []).filter(function (oItem) { return !oItem.selected; });
      aChecks.forEach(function (oItem, i) { oItem.no = i + 1; oItem.selected = false; });
      oDataModel.setProperty("/object/checks", aChecks);
      this._updateSelectionState();
    },

    onDeleteSelectedBarriers: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = (oDataModel.getProperty("/object/barriers") || []).filter(function (oItem) { return !oItem.selected; });
      aBarriers.forEach(function (oItem, i) { oItem.no = i + 1; oItem.selected = false; });
      oDataModel.setProperty("/object/barriers", aBarriers);
      this._updateSelectionState();
    },


    onDeleteCheckRow: function (oEvent) {
      var oCtx = oEvent.getSource().getBindingContext("data");
      if (!oCtx) {
        return;
      }

      var iIndex = Number(oCtx.getPath().split("/").pop());
      var sBasePath = "/object/checks";
      var oModel = this.getModel("data");
      var aItems = oModel.getProperty(sBasePath) || [];

      if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
        return;
      }

      aItems.splice(iIndex, 1);
      aItems.forEach(function (oItem, i) {
        oItem.no = i + 1;
        oItem.selected = false;
      });
      oModel.setProperty(sBasePath, aItems);
      this._updateSelectionState();
    },

    onDeleteBarrierRow: function (oEvent) {
      var oCtx = oEvent.getSource().getBindingContext("data");
      if (!oCtx) {
        return;
      }

      var iIndex = Number(oCtx.getPath().split("/").pop());
      var sBasePath = "/object/barriers";
      var oModel = this.getModel("data");
      var aItems = oModel.getProperty(sBasePath) || [];

      if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
        return;
      }

      aItems.splice(iIndex, 1);
      aItems.forEach(function (oItem, i) {
        oItem.no = i + 1;
        oItem.selected = false;
      });
      oModel.setProperty(sBasePath, aItems);
      this._updateSelectionState();
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
