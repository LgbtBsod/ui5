sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/m/MessageBox",
  "sap/ui/model/json/JSONModel",
  "sap_ui5/util/RowListHelper",
  "sap_ui5/util/ChecklistDraftHelper",
  "sap_ui5/util/FlowCoordinator"
], function (BaseController, BackendAdapter, MessageToast, MessageBox, JSONModel, RowListHelper, ChecklistDraftHelper, FlowCoordinator) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Object", {

    onInit: function () {
      this.setModel(new JSONModel({
        hasSelectedChecks: false,
        hasSelectedBarriers: false
      }), "view");
      this.attachRouteMatched("object", this._onMatched);
      this.getModel("data").attachPropertyChange(this._onDraftChanged, this);
    },

    _onDraftChanged: function (oEvent) {
      var sPath = oEvent.getParameter("path") || "";
      if (sPath.indexOf("/object") !== 0) {
        return;
      }
      this._syncDirtyFlag();
    },

    _syncDirtyFlag: function () {
      var oData = this.getModel("data");
      var oCurrent = oData.getProperty("/object");
      var oOriginal = oData.getProperty("/objectOriginal");
      this.getModel("state").setProperty("/isDirty", ChecklistDraftHelper.hasChanges(oCurrent, oOriginal));
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
        oObjectData = ChecklistDraftHelper.buildDefaultChecklist("");
      } else if (oFoundChecklist) {
        oObjectData = ChecklistDraftHelper.clone(oFoundChecklist);
      } else {
        oObjectData = ChecklistDraftHelper.buildDefaultChecklist(sId);
      }

      if (bCopy && oObjectData && oObjectData.root) {
        oObjectData.root.id = "";
      }

      oState.setProperty("/mode", (bCreate || bCopy || bEdit) ? "EDIT" : "READ");
      oState.setProperty("/activeObjectId", sId && sId !== "__create" ? sId : null);
      oState.setProperty("/objectAction", "");
      oModel.setProperty("/object", oObjectData);
      oModel.setProperty("/objectOriginal", ChecklistDraftHelper.clone(oObjectData));
      oState.setProperty("/isDirty", false);
      this._updateSelectionState();

      this.getOwnerComponent().getRootControl().byId("fcl").setLayout(oRouteArgs.layout || "TwoColumnsMidExpanded");

      if (!bCreate && !bCopy && !bEdit && !oFoundChecklist && sId) {
        this.runWithStateFlag(oState, "/isLoading", function () {
          return BackendAdapter.getCheckLists().then(function (aLoadedChecklists) {
          var aSafe = aLoadedChecklists || [];
          var oRemote = aSafe.find(function (oChecklist) {
            return oChecklist && oChecklist.root && oChecklist.root.id === sId;
          });

          if (oRemote) {
            oModel.setProperty("/checkLists", aSafe);
            oModel.setProperty("/visibleCheckLists", aSafe);
            oModel.setProperty("/object", ChecklistDraftHelper.clone(oRemote));
            oModel.setProperty("/objectOriginal", ChecklistDraftHelper.clone(oRemote));
            this.getModel("state").setProperty("/isDirty", false);
            this._updateSelectionState();
          }
          }.bind(this));
        }.bind(this));
      }
    },

    onToggleEdit: function (oEvent) {
      var isEdit = oEvent.getParameter("state");
      var oState = this.getModel("state");
      var sObjectId = oState.getProperty("/activeObjectId");
      var sSessionId = oState.getProperty("/sessionId");

      this.setLockUiState(oState, "PENDING", this.getResourceBundle().getText("lockPending"));
      this.runWithStateFlag(oState, "/lockOperationPending", function () {
        if (isEdit) {
          return BackendAdapter.lockAcquire(sObjectId, sSessionId).then(function () {
            oState.setProperty("/isLocked", true);
            oState.setProperty("/mode", "EDIT");
            this.setLockUiState(oState, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
          }.bind(this)).catch(function (oError) {
            oState.setProperty("/mode", "READ");
            oState.setProperty("/isLocked", false);
            this.setLockUiState(oState, "ERROR", this.getResourceBundle().getText("lockOwnedByOther"));
            return FlowCoordinator.handleBackendError(this, oError);
          }.bind(this));
        }

        return this.releaseLock(sObjectId, sSessionId, { trySave: true }).finally(function () {
          oState.setProperty("/isLocked", false);
          oState.setProperty("/mode", "READ");
          this.setLockUiState(oState, "IDLE", this.getResourceBundle().getText("lockReleased"));
        }.bind(this));
      }.bind(this));
    },

    onAddCheckRow: function () {
      var oDataModel = this.getModel("data");
      var aChecks = oDataModel.getProperty("/object/checks") || [];
      oDataModel.setProperty("/object/checks", RowListHelper.addRow(aChecks));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onAddBarrierRow: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];
      oDataModel.setProperty("/object/barriers", RowListHelper.addRow(aBarriers));
      this._syncDirtyFlag();
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
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteSelectedChecks: function () {
      var oDataModel = this.getModel("data");
      var aChecks = oDataModel.getProperty("/object/checks") || [];
      oDataModel.setProperty("/object/checks", RowListHelper.removeSelectedRows(aChecks));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteSelectedBarriers: function () {
      var oDataModel = this.getModel("data");
      var aBarriers = oDataModel.getProperty("/object/barriers") || [];
      oDataModel.setProperty("/object/barriers", RowListHelper.removeSelectedRows(aBarriers));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteCheckRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "data", "/object/checks");
      if (!oResult.deleted) {
        return;
      }
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteBarrierRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "data", "/object/barriers");
      if (!oResult.deleted) {
        return;
      }
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onSave: function () {
      var oDataModel = this.getModel("data");
      var oStateModel = this.getModel("state");
      var oObject = ChecklistDraftHelper.clone(oDataModel.getProperty("/object") || {});
      var sId = (((oObject || {}).root || {}).id || "").trim();
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var iExistingIndex = aChecklists.findIndex(function (oChecklist) {
        return oChecklist && oChecklist.root && oChecklist.root.id === sId;
      });
      var bCreateMode = !sId || iExistingIndex < 0;

      return this.runWithStateFlag(oStateModel, "/isBusy", function () {

      return (bCreateMode ? BackendAdapter.createCheckList(oObject) : BackendAdapter.updateCheckList(sId, oObject))
        .then(function (oSavedChecklist) {
          if (!oSavedChecklist || !oSavedChecklist.root || !oSavedChecklist.root.id) {
            throw new Error(this.getResourceBundle().getText("objectSaveInvalidResponse"));
          }
          return BackendAdapter.getCheckLists().then(function (aUpdatedCheckLists) {
            return { savedChecklist: oSavedChecklist, checkLists: aUpdatedCheckLists };
          });
        }).then(function (oResult) {
          var oSavedChecklist = oResult.savedChecklist;
          var aUpdatedCheckLists = oResult.checkLists;
          oDataModel.setProperty("/checkLists", aUpdatedCheckLists);
          oDataModel.setProperty("/visibleCheckLists", aUpdatedCheckLists);
          oDataModel.setProperty("/selectedChecklist", oSavedChecklist);
          oDataModel.setProperty("/object", ChecklistDraftHelper.clone(oSavedChecklist));
          oDataModel.setProperty("/objectOriginal", ChecklistDraftHelper.clone(oSavedChecklist));
          oStateModel.setProperty("/mode", "READ");
          oStateModel.setProperty("/isDirty", false);

          window.dispatchEvent(new CustomEvent("pcct:fullSave"));
          this.showI18nToast("objectSaved");
          this.navTo("detail", { id: oSavedChecklist.root.id });
        }.bind(this)).catch(function (oError) {
          return FlowCoordinator.handleBackendError(this, oError, {
            onConflictChoice: function (sChoice) {
              if (sChoice === this.getResourceBundle().getText("reloadButton")) {
                return BackendAdapter.getChecklistRoot(sId).then(function (oRootOnly) {
                  if (oRootOnly) {
                    oDataModel.setProperty("/object", ChecklistDraftHelper.clone(oRootOnly));
                    oDataModel.setProperty("/objectOriginal", ChecklistDraftHelper.clone(oRootOnly));
                    oStateModel.setProperty("/isDirty", false);
                  }
                });
              }
              if (sChoice === this.getResourceBundle().getText("overwriteButton")) {
                return BackendAdapter.updateCheckList(sId, oObject, { force: true });
              }
              return null;
            }
          });
        }.bind(this));
      }.bind(this));
    },

    onCancel: function () {
      var oDataModel = this.getModel("data");
      var oStateModel = this.getModel("state");
      var oOriginal = oDataModel.getProperty("/objectOriginal");
      var sOriginalId = ((((oOriginal || {}).root || {}).id) || "").trim();
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      if (!oStateModel.getProperty("/isDirty")) {
        this._restoreAndLeave(oOriginal, sOriginalId, sObjectId, sSessionId);
        return;
      }

      FlowCoordinator.confirmUnsavedAndHandle(this, this.onSave.bind(this)).then(function (sDecision) {
        if (sDecision === "DISCARD") {
          this._restoreAndLeave(oOriginal, sOriginalId, sObjectId, sSessionId);
        }
      }.bind(this));
    },

    _restoreAndLeave: function (oOriginal, sOriginalId, sObjectId, sSessionId) {
      var oDataModel = this.getModel("data");
      var oStateModel = this.getModel("state");

      if (oOriginal) {
        oDataModel.setProperty("/object", ChecklistDraftHelper.clone(oOriginal));
      }
      oStateModel.setProperty("/mode", "READ");
      oStateModel.setProperty("/isLocked", false);
      oStateModel.setProperty("/isDirty", false);
      this.releaseLock(sObjectId, sSessionId);

      if (sOriginalId) {
        this.navTo("detail", { id: sOriginalId }, true);
        return;
      }
      this.navTo("search", {}, true);
    }

  });
});
