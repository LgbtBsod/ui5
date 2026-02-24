sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/ui/model/json/JSONModel",
  "sap_ui5/util/RowListHelper"
], function (BaseController, BackendAdapter, MessageToast, JSONModel, RowListHelper) {
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
      var oRouteArgs = oEvent.getParameter("arguments") || {};
      var aChecklists = oModel.getProperty("/checkLists") || [];
      var oFoundChecklist = aChecklists.find(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      var oObjectData;
      var sAction = oState.getProperty("/objectAction") || "";
      var bCreate = sAction === "CREATE";
      var bCopy = sAction === "COPY";
      var bEdit = sAction === "EDIT";

      if (sId === "__create" && !bCreate) {
        this.getModel("state").setProperty("/layout", "OneColumn");
        this.navTo("search", {}, true);
        return;
      }

      if (bCreate || sId === "__create") {
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
      oState.setProperty("/activeObjectId", sId && sId !== "__create" ? sId : null);
      oState.setProperty("/objectAction", "");
      oModel.setProperty("/object", oObjectData);
      oModel.setProperty("/objectOriginal", _clone(oObjectData));
      this._updateSelectionState();

      this.getOwnerComponent().getRootControl().byId("fcl").setLayout(oRouteArgs.layout || "TwoColumnsMidExpanded");

      if (!bCreate && !bCopy && !bEdit && !oFoundChecklist && sId) {
        oState.setProperty("/isLoading", true);
        BackendAdapter.getCheckLists().then(function (aLoadedChecklists) {
          var aSafe = aLoadedChecklists || [];
          var oRemote = aSafe.find(function (oChecklist) {
            return oChecklist && oChecklist.root && oChecklist.root.id === sId;
          });

          if (oRemote) {
            oModel.setProperty("/checkLists", aSafe);
            oModel.setProperty("/visibleCheckLists", aSafe);
            oModel.setProperty("/object", _clone(oRemote));
            oModel.setProperty("/objectOriginal", _clone(oRemote));
            this._updateSelectionState();
          }
        }.bind(this)).finally(function () {
          oState.setProperty("/isLoading", false);
        });
      }
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      var oState = this.getModel("state");

      if (isEdit) {
        BackendAdapter.lockAcquire(
          oState.getProperty("/activeObjectId"),
          oState.getProperty("/sessionId")
        ).then(function () {
          oState.setProperty("/isLocked", true);
        }).catch(function () {
          oState.setProperty("/mode", "READ");
          oState.setProperty("/isLocked", false);
          MessageToast.show(this.getResourceBundle().getText("lockConflictMessage"));
        }.bind(this));
      } else {
        oState.setProperty("/isLocked", false);
      }

      oState.setProperty("/mode", isEdit ? "EDIT" : "READ");
    },


    onAddCheckRow: function () {
      var oDataModel = this.getModel("data");
      var aChecks = oDataModel.getProperty("/object/checks") || [];
      oDataModel.setProperty("/object/checks", RowListHelper.addRow(aChecks));
      this._updateSelectionState();
    },

    onAddBarrierRow: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];
      oDataModel.setProperty("/object/barriers", RowListHelper.addRow(aBarriers));
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
      var aChecks = oDataModel.getProperty("/object/checks") || [];
      oDataModel.setProperty("/object/checks", RowListHelper.removeSelectedRows(aChecks));
      this._updateSelectionState();
    },

    onDeleteSelectedBarriers: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];
      oDataModel.setProperty("/object/barriers", RowListHelper.removeSelectedRows(aBarriers));
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

      oModel.setProperty(sBasePath, RowListHelper.removeRowByIndex(aItems, iIndex));
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

      oModel.setProperty(sBasePath, RowListHelper.removeRowByIndex(aItems, iIndex));
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
