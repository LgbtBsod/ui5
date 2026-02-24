sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/ui/model/json/JSONModel",
  "sap_ui5/util/RowListHelper"
], function (BaseController, BackendAdapter, MessageToast, JSONModel, RowListHelper) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Detail", {

    onInit: function () {
      var oViewModel = new JSONModel({
        hasSelectedChecks: false,
        hasSelectedBarriers: false,
        infoCards: [
          { key: "observer", title: this.getResourceBundle().getText("observerLabel") },
          { key: "observed", title: this.getResourceBundle().getText("observedLabel") },
          { key: "location", title: this.getResourceBundle().getText("locationLabel") }
        ]
      });

      this.getView().setModel(oViewModel, "view");
      this.attachRouteMatched("detail", this._onMatched);
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      var oStateModel = this.getModel("state");
      oStateModel.setProperty("/layout", sLayout);
      oStateModel.setProperty("/mode", "READ");

      this._bindChecklistById(sId);
    },

    _bindChecklistById: function (sId) {
      var oDataModel = this.getModel("data");
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var oLocalMatch = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      if (oLocalMatch) {
        oDataModel.setProperty("/selectedChecklist", oLocalMatch);
        this.getModel("selected").setData(JSON.parse(JSON.stringify(oLocalMatch)));
        this._updateSelectionState();
        return;
      }

      this.getModel("state").setProperty("/isLoading", true);
      BackendAdapter.getCheckLists().then(function (aLoadedChecklists) {
        var aSafeChecklists = aLoadedChecklists || [];
        var oRemoteMatch = aSafeChecklists.find(function (item) {
          return item && item.root && item.root.id === sId;
        }) || null;

        oDataModel.setProperty("/checkLists", aSafeChecklists);
        oDataModel.setProperty("/visibleCheckLists", aSafeChecklists);
        oDataModel.setProperty("/selectedChecklist", oRemoteMatch);
        this.getModel("selected").setData(JSON.parse(JSON.stringify(oRemoteMatch || {})));
        this._updateSelectionState();
      }.bind(this)).finally(function () {
        this.getModel("state").setProperty("/isLoading", false);
      }.bind(this));
    },

    onCloseDetail: function () {
      this.getModel("state").setProperty("/layout", "OneColumn");
      this.navTo("search", {}, true);
    },

    onToggleEditFromDetail: function (oEvent) {
      var bEditMode = oEvent.getParameter("state");

      if (!bEditMode) {
        this.getModel("state").setProperty("/mode", "READ");
        return;
      }

      this.getModel("state").setProperty("/mode", "EDIT");
    },

    onCancelEditFromDetail: function () {
      var oSelected = this.getModel("data").getProperty("/selectedChecklist") || {};
      this.getModel("selected").setData(JSON.parse(JSON.stringify(oSelected)));
      this.getModel("state").setProperty("/mode", "READ");
    },


    _updateSelectionState: function () {
      var oSelectedModel = this.getModel("selected");
      var oViewModel = this.getView().getModel("view");
      var aChecks = oSelectedModel.getProperty("/checks") || [];
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];

      if (!oViewModel) {
        return;
      }

      oViewModel.setProperty("/hasSelectedChecks", aChecks.some(function (oItem) { return !!(oItem && oItem.selected); }));
      oViewModel.setProperty("/hasSelectedBarriers", aBarriers.some(function (oItem) { return !!(oItem && oItem.selected); }));
    },

    onDeleteSelectedChecks: function () {
      var oSelectedModel = this.getModel("selected");
      var aChecks = oSelectedModel.getProperty("/checks") || [];
      oSelectedModel.setProperty("/checks", RowListHelper.removeSelectedRows(aChecks));
      this._updateSelectionState();
    },

    onDeleteSelectedBarriers: function () {
      var oSelectedModel = this.getModel("selected");
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];
      oSelectedModel.setProperty("/barriers", RowListHelper.removeSelectedRows(aBarriers));
      this._updateSelectionState();
    },

    onSelectionToggle: function () {
      this._updateSelectionState();
    },

    formatInfoCardValue: function (sKey, sObserver, sObserved, sLocationName, sLocationText) {
      if (sKey === "observer") {
        return sObserver || "-";
      }

      if (sKey === "observed") {
        return sObserved || "-";
      }

      if (sLocationName && sLocationText && sLocationName !== sLocationText) {
        return sLocationName + " — " + sLocationText;
      }

      return sLocationName || sLocationText || "-";
    },

    onInfoCardsDrop: function (oEvent) {
      var oDragged = oEvent.getParameter("draggedControl");
      var oDropped = oEvent.getParameter("droppedControl");
      var sDropPosition = oEvent.getParameter("dropPosition");

      if (!oDragged || !oDropped) {
        return;
      }

      var oDraggedCtx = oDragged.getBindingContext("view");
      var oDroppedCtx = oDropped.getBindingContext("view");

      if (!oDraggedCtx || !oDroppedCtx) {
        return;
      }

      var iDragged = Number(oDraggedCtx.getPath().split("/").pop());
      var iDropped = Number(oDroppedCtx.getPath().split("/").pop());

      if (!Number.isInteger(iDragged) || !Number.isInteger(iDropped) || iDragged === iDropped) {
        return;
      }

      var oViewModel = this.getModel("view");
      var aItems = (oViewModel.getProperty("/infoCards") || []).slice();
      var oMoved = aItems.splice(iDragged, 1)[0];
      if (!oMoved) {
        return;
      }

      var iTarget = sDropPosition === "After" ? iDropped + 1 : iDropped;

      if (iDragged < iDropped) {
        iTarget -= 1;
      }

      aItems.splice(iTarget, 0, oMoved);
      oViewModel.setProperty("/infoCards", aItems);
    },

    onSaveDetail: function () {
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var oBundle = this.getResourceBundle();
      var oEdited = this.getModel("selected").getData() || {};
      var sId = ((((oEdited || {}).root || {}).id) || "").trim();

      if (!sId) {
        MessageToast.show(oBundle.getText("checklistIdMissing"));
        return;
      }

      oStateModel.setProperty("/isBusy", true);

      BackendAdapter.updateCheckList(sId, oEdited)
        .then(function (oSavedChecklist) {
          return BackendAdapter.getCheckLists().then(function (aUpdatedCheckLists) {
            return {
              savedChecklist: oSavedChecklist,
              checkLists: aUpdatedCheckLists
            };
          });
        })
        .then(function (oResult) {
          oDataModel.setProperty("/checkLists", oResult.checkLists);
          oDataModel.setProperty("/visibleCheckLists", oResult.checkLists);
          oDataModel.setProperty("/selectedChecklist", oResult.savedChecklist);
          this.getModel("selected").setData(oResult.savedChecklist);
          oStateModel.setProperty("/mode", "READ");
          MessageToast.show(oBundle.getText("objectSaved"));
        }.bind(this))
        .catch(function (oError) {
          MessageToast.show(oBundle.getText("objectSaveFailed", [((oError && oError.message) || "Unknown error")]));
        })
        .finally(function () {
          oStateModel.setProperty("/isBusy", false);
        });
    },

    onAddCheckRow: function () {
      var oSelectedModel = this.getModel("selected");
      var aChecks = oSelectedModel.getProperty("/checks") || [];
      oSelectedModel.setProperty("/checks", RowListHelper.addRow(aChecks));
      this._updateSelectionState();
    },

    onAddBarrierRow: function () {
      var oSelectedModel = this.getModel("selected");
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];
      oSelectedModel.setProperty("/barriers", RowListHelper.addRow(aBarriers));
      this._updateSelectionState();
    },


    onDeleteCheckRow: function (oEvent) {
      var oCtx = oEvent.getSource().getBindingContext("selected");
      if (!oCtx) {
        return;
      }

      var iIndex = Number(oCtx.getPath().split("/").pop());
      var sBasePath = "/checks";
      var oModel = this.getModel("selected");
      var aItems = oModel.getProperty(sBasePath) || [];

      if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
        return;
      }

      oModel.setProperty(sBasePath, RowListHelper.removeRowByIndex(aItems, iIndex));
      this._updateSelectionState();
    },

    onDeleteBarrierRow: function (oEvent) {
      var oCtx = oEvent.getSource().getBindingContext("selected");
      if (!oCtx) {
        return;
      }

      var iIndex = Number(oCtx.getPath().split("/").pop());
      var sBasePath = "/barriers";
      var oModel = this.getModel("selected");
      var aItems = oModel.getProperty(sBasePath) || [];

      if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
        return;
      }

      oModel.setProperty(sBasePath, RowListHelper.removeRowByIndex(aItems, iIndex));
      this._updateSelectionState();
    },

    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }

  });
});
