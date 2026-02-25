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

  return BaseController.extend("sap_ui5.controller.Detail", {

    onInit: function () {
      var oViewModel = new JSONModel({
        hasSelectedChecks: false,
        hasSelectedBarriers: false,
        observerSuggestions: [],
        observedSuggestions: [],
        locationTree: [],
        checksLoading: false,
        barriersLoading: false,
        infoCards: [
          { key: "observer", title: this.getResourceBundle().getText("observerLabel") },
          { key: "observed", title: this.getResourceBundle().getText("observedLabel") },
          { key: "location", title: this.getResourceBundle().getText("locationLabel") }
        ]
      });

      this.getView().setModel(oViewModel, "view");
      this.attachRouteMatched("detail", this._onMatched);
      this.getModel("selected").attachPropertyChange(this._onSelectedChanged, this);
      this.getModel("mpl").attachPropertyChange(this._onMplChanged, this);
    },

    _onMplChanged: function (oEvent) {
      if ((oEvent.getParameter("path") || "") !== "/locations") {
        return;
      }
      this._prepareLocationTree();
    },

    _onSelectedChanged: function (oEvent) {
      var sPath = oEvent.getParameter("path") || "";
      if (sPath.indexOf("/") !== 0) {
        return;
      }
      this._syncDirtyFlag();
    },

    _syncDirtyFlag: function () {
      var oEdited = this.getModel("selected").getData() || {};
      var oOriginal = this.getModel("data").getProperty("/selectedChecklist") || {};
      this.getModel("state").setProperty("/isDirty", ChecklistDraftHelper.hasChanges(oEdited, oOriginal));
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      var oStateModel = this.getModel("state");
      oStateModel.setProperty("/layout", sLayout);
      oStateModel.setProperty("/mode", "READ");
      oStateModel.setProperty("/activeObjectId", sId || null);

      this._prepareLocationTree();
      this._bindChecklistById(sId);
      oStateModel.setProperty("/isDirty", false);
    },

    _prepareLocationTree: function () {
      var aLocations = this.getModel("mpl").getProperty("/locations") || [];
      var aTree = this._buildLocationTree(aLocations);
      this.getModel("view").setProperty("/locationTree", aTree);
    },

    _buildLocationTree: function (aFlatNodes) {
      var mById = {};
      var aRoots = [];

      (aFlatNodes || []).forEach(function (oNode) {
        var sNodeId = oNode && oNode.node_id;
        if (!sNodeId) {
          return;
        }
        mById[sNodeId] = {
          node_id: sNodeId,
          parent_id: oNode.parent_id || null,
          location_name: oNode.location_name || sNodeId,
          hierarchy_level: oNode.hierarchy_level,
          children: []
        };
      });

      Object.keys(mById).forEach(function (sId) {
        var oNode = mById[sId];
        if (oNode.parent_id && mById[oNode.parent_id]) {
          mById[oNode.parent_id].children.push(oNode);
        } else {
          aRoots.push(oNode);
        }
      });

      return aRoots;
    },

    _bindChecklistById: function (sId) {
      var oDataModel = this.getModel("data");
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var oLocalMatch = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      if (oLocalMatch) {
        oDataModel.setProperty("/selectedChecklist", oLocalMatch);
        this._applyChecklistLazily(oLocalMatch);
        return;
      }

      this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
        return BackendAdapter.getChecklistRoot(sId).then(function (oRootOnly) {
          oDataModel.setProperty("/selectedChecklist", oRootOnly);
          this._applyChecklistLazily(oRootOnly);
        }.bind(this));
      }.bind(this));
    },



    _applyChecklistLazily: function (oChecklist) {
      var oView = this.getModel("view");
      var oSelected = ChecklistDraftHelper.clone(oChecklist || {});
      var sId = (((oSelected || {}).root || {}).id) || "";

      oSelected.checks = [];
      oSelected.barriers = [];
      this.getModel("selected").setData(oSelected);
      oView.setProperty("/checksLoading", true);
      oView.setProperty("/barriersLoading", true);

      if (!sId) {
        oView.setProperty("/checksLoading", false);
        oView.setProperty("/barriersLoading", false);
        this._updateSelectionState();
        return;
      }

      // Fast root/basic first, then sequential lazy requests (checks -> barriers) without UI-level expand binding.
      BackendAdapter.getChecklistChecks(sId).then(function (aChecks) {
        this.getModel("selected").setProperty("/checks", aChecks || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/checksLoading", false);
        this._updateSelectionState();
      }.bind(this));

      BackendAdapter.getChecklistBarriers(sId).then(function (aBarriers) {
        this.getModel("selected").setProperty("/barriers", aBarriers || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/barriersLoading", false);
        this._updateSelectionState();
      }.bind(this));
    },

    onPersonSuggest: function (oEvent) {
      var sValue = this._normalizeText(oEvent.getParameter("suggestValue"));
      var sTarget = oEvent.getSource().data("target");
      var aPersons = this.getModel("masterData").getProperty("/persons") || [];
      var aFiltered = aPersons.filter(function (oPerson) {
        var sName = this._normalizeText(oPerson.fullName);
        var sPernr = this._normalizeText(oPerson.perner);
        var sPosition = this._normalizeText(oPerson.position);
        return !sValue || sName.indexOf(sValue) >= 0 || sPernr.indexOf(sValue) >= 0 || sPosition.indexOf(sValue) >= 0;
      }.bind(this)).slice(0, 10);

      this._setPersonSuggestions(sTarget, aFiltered);

      if (sValue && aFiltered.length < 3 && BackendAdapter.suggestPersons) {
        BackendAdapter.suggestPersons(sValue).then(function (aRemote) {
          this._setPersonSuggestions(sTarget, (aRemote || []).slice(0, 10));
        }.bind(this));
      }
    },

    _setPersonSuggestions: function (sTarget, aSuggestions) {
      this.getModel("view").setProperty(sTarget === "observed" ? "/observedSuggestions" : "/observerSuggestions", aSuggestions || []);
    },

    _normalizeText: function (sValue) {
      return String(sValue || "").trim().toLowerCase();
    },

    onPersonSuggestionSelected: function (oEvent) {
      var oItem = oEvent.getParameter("selectedItem");
      if (!oItem) {
        return;
      }

      var sTarget = oEvent.getSource().data("target");
      var oPerson = oItem.getBindingContext("view").getObject();
      var sPrefix = sTarget === "observed" ? "OBSERVED_" : "OBSERVER_";
      var oSelectedModel = this.getModel("selected");

      oSelectedModel.setProperty("/basic/" + sPrefix + "PERNER", oPerson.perner || "");
      oSelectedModel.setProperty("/basic/" + sPrefix + "FULLNAME", oPerson.fullName || "");
      oSelectedModel.setProperty("/basic/" + sPrefix + "POSITION", oPerson.position || "");
      oSelectedModel.setProperty("/basic/" + sPrefix + "ORGUNIT", oPerson.orgUnit || "");
      oSelectedModel.setProperty("/basic/" + sPrefix + "INTEGRATION_NAME", oPerson.integrationName || "");
    },

    onLocationSelectionChange: function (oEvent) {
      var oItem = oEvent.getParameter("listItem");
      if (!oItem) {
        return;
      }

      var oNode = oItem.getBindingContext("view").getObject();
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/basic/LOCATION_KEY", oNode.node_id || "");
      oSelectedModel.setProperty("/basic/LOCATION_NAME", oNode.location_name || "");
      oSelectedModel.setProperty("/basic/LOCATION_TEXT", oNode.location_name || "");
    },

    onCloseDetail: function () {
      var oState = this.getModel("state");
      var fnProceed = function () {
        var sObjectId = oState.getProperty("/activeObjectId");
        var sSessionId = oState.getProperty("/sessionId");
        this.releaseLock(sObjectId, sSessionId);
        oState.setProperty("/layout", "OneColumn");
        oState.setProperty("/activeObjectId", null);
        oState.setProperty("/isDirty", false);
        this.navTo("search", {}, true);
      }.bind(this);

      if (!oState.getProperty("/isDirty")) {
        fnProceed();
        return;
      }

      FlowCoordinator.confirmUnsavedAndHandle(this, this.onSaveDetail.bind(this)).then(function (sDecision) {
        if (sDecision === "DISCARD") {
          fnProceed();
        }
      });
    },

    onToggleEditFromDetail: function (oEvent) {
      var bEditMode = oEvent.getParameter("state");
      var oStateModel = this.getModel("state");
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      this.setLockUiState(oStateModel, "PENDING", this.getResourceBundle().getText("lockPending"));
      this.runWithStateFlag(oStateModel, "/lockOperationPending", function () {
        if (!bEditMode) {
          return this.releaseLock(sObjectId, sSessionId).finally(function () {
            oStateModel.setProperty("/mode", "READ");
            oStateModel.setProperty("/isLocked", false);
            this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockReleased"));
          }.bind(this));
        }

        return BackendAdapter.lockAcquire(
          sObjectId,
          sSessionId
        ).then(function () {
          oStateModel.setProperty("/isLocked", true);
          oStateModel.setProperty("/mode", "EDIT");
          this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        }.bind(this)).catch(function (oError) {
          oStateModel.setProperty("/mode", "READ");
          oStateModel.setProperty("/isLocked", false);
          this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockOwnedByOther"));
          return FlowCoordinator.handleBackendError(this, oError);
        }.bind(this));
      }.bind(this));
    },

    onCancelEditFromDetail: function () {
      var oSelected = this.getModel("data").getProperty("/selectedChecklist") || {};
      this.getModel("selected").setData(ChecklistDraftHelper.clone(oSelected));
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
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteSelectedBarriers: function () {
      var oSelectedModel = this.getModel("selected");
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];
      oSelectedModel.setProperty("/barriers", RowListHelper.removeSelectedRows(aBarriers));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onSelectionToggle: function () {
      this._syncDirtyFlag();
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

      iTarget = Math.max(0, Math.min(iTarget, aItems.length));
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
        this.showI18nToast("checklistIdMissing");
        return;
      }

      return this.runWithStateFlag(oStateModel, "/isBusy", function () {

      return BackendAdapter.updateCheckList(sId, oEdited)
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
          oStateModel.setProperty("/isDirty", false);
          window.dispatchEvent(new CustomEvent("pcct:fullSave"));
          this.showI18nToast("objectSaved");
        }.bind(this))
        .catch(function (oError) {
          return FlowCoordinator.handleBackendError(this, oError, {
            onConflictChoice: function (sChoice) {
              if (sChoice === this.getResourceBundle().getText("reloadButton")) {
                return BackendAdapter.getChecklistRoot(sId).then(function (oRootOnly) {
                  if (oRootOnly) {
                    this.getModel("selected").setData(ChecklistDraftHelper.clone(oRootOnly));
                    this._applyChecklistLazily(oRootOnly);
                  }
                }.bind(this));
              }
              if (sChoice === this.getResourceBundle().getText("overwriteButton")) {
                return BackendAdapter.updateCheckList(sId, oEdited, { force: true });
              }
              return null;
            }.bind(this)
          });
        }.bind(this));
      }.bind(this));
    },

    onAddCheckRow: function () {
      var oSelectedModel = this.getModel("selected");
      var aChecks = oSelectedModel.getProperty("/checks") || [];
      oSelectedModel.setProperty("/checks", RowListHelper.addRow(aChecks));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onAddBarrierRow: function () {
      var oSelectedModel = this.getModel("selected");
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];
      oSelectedModel.setProperty("/barriers", RowListHelper.addRow(aBarriers));
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteCheckRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "selected", "/checks");
      if (!oResult.deleted) {
        return;
      }
      this._syncDirtyFlag();
      this._updateSelectionState();
    },

    onDeleteBarrierRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "selected", "/barriers");
      if (!oResult.deleted) {
        return;
      }
      this._syncDirtyFlag();
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
