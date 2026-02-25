sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/m/MessageBox",
  "sap/ui/model/json/JSONModel",
  "sap/ui/core/Fragment",
  "sap_ui5/util/RowListHelper",
  "sap_ui5/util/ChecklistDraftHelper",
  "sap_ui5/util/FlowCoordinator"
], function (BaseController, BackendAdapter, MessageToast, MessageBox, JSONModel, Fragment, RowListHelper, ChecklistDraftHelper, FlowCoordinator) {
  "use strict";

  return BaseController.extend("sap_ui5.controller.Detail", {

    _getCurrentUser: function () {
      var oState = this.getModel("state");
      return oState.getProperty("/testUser") || oState.getProperty("/testUserLogin") || "demoUser";
    },

    _tryStealOwnLock: function (sObjectId, sSessionId, oError) {
      var oStateModel = this.getModel("state");
      var oDetail = FlowCoordinator.extractBackendDetail(oError) || {};
      var bOwnLockConflict = oDetail.action === "LOCKED"
        && oDetail.owner
        && oDetail.owner === this._getCurrentUser()
        && oDetail.owner_session;

      if (!bOwnLockConflict) {
        return Promise.reject(oError);
      }

      return FlowCoordinator.confirmStealOwnLock(this).then(function (bShouldSteal) {
        if (!bShouldSteal) {
          return Promise.resolve({ success: false });
        }

        return BackendAdapter.lockAcquire(
          sObjectId,
          sSessionId,
          this._getCurrentUser(),
          oDetail.owner_session
        );
      }.bind(this)).then(function (oResult) {
        if (!oResult || !oResult.success) {
          oStateModel.setProperty("/mode", "READ");
          oStateModel.setProperty("/isLocked", false);
          this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockStayReadOnly"));
          return null;
        }

        oStateModel.setProperty("/isLocked", true);
        oStateModel.setProperty("/mode", "EDIT");
        this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        return oResult;
      }.bind(this));
    },

    onInit: function () {
      var oViewModel = new JSONModel({
        hasSelectedChecks: false,
        hasSelectedBarriers: false,
        observerSuggestions: [],
        observedSuggestions: [],
        locationTree: [],
        checksLoading: false,
        barriersLoading: false,
        detailSkeletonBusy: false,
        infoCards: [
          { key: "observer", title: this.getResourceBundle().getText("observerLabel") },
          { key: "observed", title: this.getResourceBundle().getText("observedLabel") },
          { key: "location", title: this.getResourceBundle().getText("locationLabel") },
          { key: "lpc", title: this.getResourceBundle().getText("lpcLabel") },
          { key: "profession", title: this.getResourceBundle().getText("professionLabel") }
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
      if ((oEdited.checks || []).some(function (oRow) { return oRow && oRow.__loading; })
        || (oEdited.barriers || []).some(function (oRow) { return oRow && oRow.__loading; })) {
        return;
      }
      this.getModel("state").setProperty("/isDirty", ChecklistDraftHelper.hasChanges(oEdited, oOriginal));
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var sLayout = oEvent.getParameter("arguments").layout || "TwoColumnsMidExpanded";
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var sAction = oStateModel.getProperty("/objectAction") || "";
      var bCreate = sAction === "CREATE" || sId === "__create";
      var bCopy = sAction === "COPY";

      oStateModel.setProperty("/layout", sLayout);
      oStateModel.setProperty("/activeObjectId", bCreate ? null : (sId || null));
      oStateModel.setProperty("/objectAction", "");

      this._prepareLocationTree();

      if (bCreate) {
        // Single-card flow: create is handled on detail card directly.
        var oDraft = ChecklistDraftHelper.buildDefaultChecklist("");
        oDataModel.setProperty("/selectedChecklist", oDraft);
        this.getModel("selected").setData(ChecklistDraftHelper.clone(oDraft));
        oStateModel.setProperty("/mode", "EDIT");
        oStateModel.setProperty("/isDirty", false);
        this._updateSelectionState();
        return;
      }

      if (bCopy) {
        var oSeed = this.getModel("selected").getData() || oDataModel.getProperty("/selectedChecklist") || {};
        var oCopy = ChecklistDraftHelper.clone(oSeed);
        if (!oCopy.root) { oCopy.root = {}; }
        oCopy.root.id = "";
        oDataModel.setProperty("/selectedChecklist", oCopy);
        this.getModel("selected").setData(ChecklistDraftHelper.clone(oCopy));
        oStateModel.setProperty("/mode", "EDIT");
        oStateModel.setProperty("/isDirty", true);
        this._updateSelectionState();
        return;
      }

      oStateModel.setProperty("/mode", "READ");
      this._bindChecklistById(sId);
      oStateModel.setProperty("/isDirty", false);
    },

    _prepareLocationTree: function () {
      var aLocations = this.getModel("mpl").getProperty("/locations") || [];
      var aTree = this._buildLocationTree(aLocations);
      var oView = this.getView();
      var oViewModel = oView && oView.getModel("view");
      if (!oViewModel) {
        oViewModel = new JSONModel({});
        oView.setModel(oViewModel, "view");
      }
      oViewModel.setProperty("/locationTree", aTree);
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



    _isChecklistCacheValid: function () {
      var oComp = this.getOwnerComponent();
      return !!(oComp && oComp._oSmartCache && oComp._oSmartCache.isCacheValid && oComp._oSmartCache.isCacheValid("checkLists"));
    },

    _reloadChecklistFromBackend: function (sId) {
      var oDataModel = this.getModel("data");
      return this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
        return BackendAdapter.getChecklistRoot(sId).then(function (oRootOnly) {
          oDataModel.setProperty("/selectedChecklist", oRootOnly || null);
          this._applyChecklistLazily(oRootOnly || {});
          return oRootOnly;
        }.bind(this));
      }.bind(this));
    },

    _ensureChecklistFreshBeforeEdit: function (sId) {
      if (!sId) {
        return Promise.resolve(null);
      }
      if (this._isChecklistCacheValid()) {
        return Promise.resolve(this.getModel("data").getProperty("/selectedChecklist") || null);
      }
      return this._reloadChecklistFromBackend(sId);
    },

    _bindChecklistById: function (sId) {
      var oDataModel = this.getModel("data");
      var aChecklists = oDataModel.getProperty("/checkLists") || [];
      var oLocalMatch = aChecklists.find(function (item) {
        return item && item.root && item.root.id === sId;
      }) || null;

      if (oLocalMatch && this._isChecklistCacheValid()) {
        // Use cache when still valid (30 seconds window).
        oDataModel.setProperty("/selectedChecklist", oLocalMatch);
        this._applyChecklistLazily(oLocalMatch);
        return;
      }

      this._reloadChecklistFromBackend(sId);
    },



    _buildSkeletonRows: function (iCount) {
      return Array.from({ length: iCount }, function (_, iIndex) {
        return {
          id: "skeleton-" + (iIndex + 1),
          text: "",
          comment: "",
          result: false,
          __loading: true
        };
      });
    },

    _loadSectionPaged: function (sId, sSection, iPageSize) {
      var fnLoader = sSection === "checks" ? BackendAdapter.getChecklistChecks : BackendAdapter.getChecklistBarriers;
      var aAllRows = [];
      var iSkip = 0;
      var fnNext = function () {
        return fnLoader.call(BackendAdapter, sId, { top: iPageSize, skip: iSkip }).then(function (aRows) {
          var aChunk = aRows || [];
          aAllRows = aAllRows.concat(aChunk);
          iSkip += aChunk.length;
          if (aChunk.length < iPageSize) {
            return aAllRows;
          }
          return fnNext();
        });
      };
      return fnNext();
    },

    _applyChecklistLazily: function (oChecklist) {
      var oView = this.getView().getModel("view");
      if (!oView) {
        return;
      }
      var oSelected = ChecklistDraftHelper.clone(oChecklist || {});
      var sId = (((oSelected || {}).root || {}).id) || "";
      var iPageSize = 20;

      oSelected.checks = this._buildSkeletonRows(iPageSize);
      oSelected.barriers = this._buildSkeletonRows(iPageSize);
      this.getModel("selected").setData(oSelected);
      oView.setProperty("/checksLoading", true);
      oView.setProperty("/barriersLoading", true);
      oView.setProperty("/detailSkeletonBusy", true);

      if (!sId) {
        oView.setProperty("/checksLoading", false);
        oView.setProperty("/barriersLoading", false);
        oView.setProperty("/detailSkeletonBusy", false);
        this._updateSelectionState();
        return;
      }

      this._loadSectionPaged(sId, "checks", iPageSize).then(function (aChecks) {
        this.getModel("selected").setProperty("/checks", aChecks || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/checksLoading", false);
        if (!oView.getProperty("/barriersLoading")) {
          oView.setProperty("/detailSkeletonBusy", false);
        }
        this._updateSelectionState();
      }.bind(this));

      this._loadSectionPaged(sId, "barriers", iPageSize).then(function (aBarriers) {
        this.getModel("selected").setProperty("/barriers", aBarriers || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/barriersLoading", false);
        if (!oView.getProperty("/checksLoading")) {
          oView.setProperty("/detailSkeletonBusy", false);
        }
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
        }.bind(this)).catch(function () {
          // Keep local suggestions only when remote suggest is unavailable.
        });
      }
    },

    _setPersonSuggestions: function (sTarget, aSuggestions) {
      var oViewModel = this.getView().getModel("view");
      if (!oViewModel) {
        return;
      }
      oViewModel.setProperty(sTarget === "observed" ? "/observedSuggestions" : "/observerSuggestions", aSuggestions || []);
    },

    _normalizeText: function (sValue) {
      return String(sValue || "").trim().toLowerCase();
    },

    formatPersonSuggestion: function (sFullName, sPosition) {
      if (!sPosition) {
        return sFullName || "";
      }
      return (sFullName || "") + " — " + sPosition;
    },

    onPersonSuggestionSelected: function (oEvent) {
      var oItem = oEvent.getParameter("selectedItem");
      if (!oItem) {
        return;
      }

      var sTarget = oEvent.getSource().data("target");
      var oCtx = oItem.getBindingContext("view");
      var oPerson = oCtx ? oCtx.getObject() : null;
      if (!oPerson) {
        return;
      }
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

      var oCtx = oItem.getBindingContext("view");
      var oNode = oCtx ? oCtx.getObject() : null;
      if (!oNode) {
        return;
      }
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/basic/LOCATION_KEY", oNode.node_id || "");
      oSelectedModel.setProperty("/basic/LOCATION_NAME", oNode.location_name || "");
      oSelectedModel.setProperty("/basic/LOCATION_TEXT", oNode.location_name || "");
    },

    onLpcChange: function (oEvent) {
      var sKey = oEvent.getParameter("selectedItem") ? oEvent.getParameter("selectedItem").getKey() : oEvent.getSource().getSelectedKey();
      var aLpc = this.getModel("masterData").getProperty("/lpc") || [];
      var oMatch = aLpc.find(function (oItem) { return oItem && oItem.key === sKey; }) || {};
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/basic/LPC_KEY", sKey || "");
      oSelectedModel.setProperty("/basic/LPC_TEXT", oMatch.text || "");
    },

    onProfessionChange: function (oEvent) {
      var sKey = oEvent.getParameter("selectedItem") ? oEvent.getParameter("selectedItem").getKey() : oEvent.getSource().getSelectedKey();
      var aProf = this.getModel("masterData").getProperty("/professions") || [];
      var oMatch = aProf.find(function (oItem) { return oItem && oItem.key === sKey; }) || {};
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/basic/PROF_KEY", sKey || "");
      oSelectedModel.setProperty("/basic/PROF_TEXT", oMatch.text || "");
    },

    _confirmIntegrationEdit: function () {
      var oRoot = this.getModel("selected").getProperty("/root") || this.getModel("data").getProperty("/selectedChecklist/root") || {};
      var bIntegrationData = !!(oRoot.this_is_integration_data || oRoot.integrationFlag);
      if (!bIntegrationData) {
        return Promise.resolve(true);
      }

      var oBundle = this.getResourceBundle();
      return new Promise(function (resolve) {
        MessageBox.warning(oBundle.getText("integrationEditWarning"), {
          title: oBundle.getText("integrationEditTitle"),
          actions: [MessageBox.Action.YES, MessageBox.Action.NO],
          emphasizedAction: MessageBox.Action.NO,
          onClose: function (sAction) {
            resolve(sAction === MessageBox.Action.YES);
          }
        });
      });
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

    onToggleEdit: function (oEvent) {
      return this.onToggleEditFromDetail(oEvent);
    },

    onToggleEditFromDetail: function (oEvent) {
      var bEditMode = oEvent.getParameter("state");
      var oStateModel = this.getModel("state");
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      this.setLockUiState(oStateModel, "PENDING", this.getResourceBundle().getText("lockPending"));
      this.runWithStateFlag(oStateModel, "/lockOperationPending", function () {
        if (!bEditMode) {
          // First switch UI to read-only so periodic probes stop immediately.
          oStateModel.setProperty("/mode", "READ");
          oStateModel.setProperty("/isLocked", false);
          return this.releaseLock(sObjectId, sSessionId).finally(function () {
            this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockReleased"));
          }.bind(this));
        }

        return this._ensureChecklistFreshBeforeEdit(sObjectId).then(function () {
          return this._confirmIntegrationEdit();
        }.bind(this)).then(function (bConfirmed) {
          if (!bConfirmed) {
            this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockStayReadOnly"));
            return null;
          }
          return BackendAdapter.lockAcquire(
            sObjectId,
            sSessionId
          );
        }.bind(this)).then(function (oAcquireResult) {
          if (!oAcquireResult) {
            return;
          }
          oStateModel.setProperty("/isLocked", true);
          oStateModel.setProperty("/mode", "EDIT");
          this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        }.bind(this)).catch(function (oError) {
          return this._tryStealOwnLock(sObjectId, sSessionId, oError).catch(function () {
            oStateModel.setProperty("/mode", "READ");
            oStateModel.setProperty("/isLocked", false);
            this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockOwnedByOther"));
            return FlowCoordinator.handleBackendError(this, oError);
          }.bind(this));
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

    formatInfoCardValue: function (sKey, sObserver, sObserved, sLocationName, sLocationText, sLpcText, sProfText) {
      if (sKey === "observer") {
        return sObserver || "-";
      }

      if (sKey === "observed") {
        return sObserved || "-";
      }

      if (sKey === "lpc") {
        return sLpcText || "-";
      }

      if (sKey === "profession") {
        return sProfText || "-";
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

      var oViewModel = this.getView().getModel("view");
      if (!oViewModel) {
        return;
      }
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

      var bCreateMode = !sId;

      return this.runWithStateFlag(oStateModel, "/isLoading", function () {

      return (bCreateMode ? BackendAdapter.createCheckList(oEdited) : BackendAdapter.updateCheckList(sId, oEdited))
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



    _openExpandedDialog: function (sType) {
      var sProp = sType === "checks" ? "_pChecksDialog" : "_pBarriersDialog";
      var sFragment = sType === "checks"
        ? "sap_ui5.view.fragment.ChecksExpandedDialog"
        : "sap_ui5.view.fragment.BarriersExpandedDialog";

      if (!this[sProp]) {
        this[sProp] = Fragment.load({
          id: this.getView().getId(),
          name: sFragment,
          controller: this
        }).then(function (oDialog) {
          this.getView().addDependent(oDialog);
          return oDialog;
        }.bind(this));
      }

      return this[sProp].then(function (oDialog) {
        oDialog.open();
      });
    },

    onExpandChecks: function () {
      this._openExpandedDialog("checks");
    },

    onExpandBarriers: function () {
      this._openExpandedDialog("barriers");
    },

    onCloseChecksExpanded: function () {
      var oDialog = this.byId("checksExpandedDialog");
      if (oDialog) {
        oDialog.close();
      }
    },

    onCloseBarriersExpanded: function () {
      var oDialog = this.byId("barriersExpandedDialog");
      if (oDialog) {
        oDialog.close();
      }
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

    onExit: function () {
      ["checksExpandedDialog", "barriersExpandedDialog"].forEach(function (sId) {
        var oDialog = this.byId(sId);
        if (oDialog) {
          oDialog.destroy();
        }
      }.bind(this));
      this._pChecksDialog = null;
      this._pBarriersDialog = null;
    },

    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }

  });
});
