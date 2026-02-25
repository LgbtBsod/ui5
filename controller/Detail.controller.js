sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/m/MessageBox",
  "sap/ui/model/json/JSONModel",
  "sap/ui/core/Fragment",
  "sap_ui5/util/RowListHelper",
  "sap_ui5/util/ChecklistDraftHelper",
  "sap_ui5/util/FlowCoordinator",
  "sap_ui5/util/ChecklistValidationService",
  "sap_ui5/util/ChecklistUiState"
], function (BaseController, BackendAdapter, MessageToast, MessageBox, JSONModel, Fragment, RowListHelper, ChecklistDraftHelper, FlowCoordinator, ChecklistValidationService, ChecklistUiState) {
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

      return FlowCoordinator.confirmStealOwnLock(this).then((bShouldSteal) => {
        if (!bShouldSteal) {
          return Promise.resolve({ success: false });
        }

        return BackendAdapter.lockAcquire(
          sObjectId,
          sSessionId,
          this._getCurrentUser(),
          oDetail.owner_session
        );
      }).then((oResult) => {
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
      });
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
        validationShown: false,
        validationMissing: {},
        statusActions: [
          { key: "DRAFT", title: "Draft" },
          { key: "REGISTERED", title: "Registered" },
          { key: "CLOSED", title: "Closed" }
        ],
        infoCards: [
          { key: "date", title: this.getResourceBundle().getText("dateLabel") },
          { key: "time", title: this.getResourceBundle().getText("timeLabel") },
          { key: "timezone", title: this.getResourceBundle().getText("timezoneLabel") },
          { key: "equipment", title: this.getResourceBundle().getText("equipmentLabel") },
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
      this._refreshValidationHighlights();
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

    _syncSelectionMeta: function () {
      this._syncDirtyFlag();
      this._updateSelectionState();
      this._updateDerivedRootResult();
    },

    _closeExpandedDialogById: function (sDialogId) {
      var oDialog = this.byId(sDialogId);
      if (oDialog) {
        oDialog.close();
      }
    },

    _releaseEditLock: function (sObjectId, sSessionId, oStateModel) {
      oStateModel.setProperty("/mode", "READ");
      oStateModel.setProperty("/isLocked", false);
      return this.releaseLock(sObjectId, sSessionId, { trySave: false }).finally(() => {
        this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockReleased"));
      });
    },

    _setBasicTextByDictionary: function (oEvent, sDictionaryPath, sKeyPath, sTextPath) {
      var sKey = oEvent.getParameter("selectedItem")
        ? oEvent.getParameter("selectedItem").getKey()
        : oEvent.getSource().getSelectedKey();
      var aDict = this.getModel("masterData").getProperty(sDictionaryPath) || [];
      var oMatch = aDict.find((oItem) => oItem && oItem.key === sKey) || {};
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty(sKeyPath, sKey || "");
      oSelectedModel.setProperty(sTextPath, oMatch.text || "");
    },

    _mutateRows: function (sPath, fnMutator) {
      var oSelectedModel = this.getModel("selected");
      var aRows = oSelectedModel.getProperty(sPath) || [];
      oSelectedModel.setProperty(sPath, fnMutator(aRows));
      this._syncSelectionMeta();
    },


    _refreshValidationHighlights: function () {
      var oViewModel = this.getView().getModel("view");
      if (!oViewModel || !oViewModel.getProperty("/validationShown")) {
        return;
      }

      var oValidation = this._runChecklistValidation();
      oViewModel.setProperty("/validationMissing", ChecklistUiState.buildValidationMap(oValidation.missingPaths));
      if (oValidation.valid) {
        oViewModel.setProperty("/validationShown", false);
      }
    },

    formatValidationState: function (bShown, bMissing) {
      return (bShown && bMissing) ? "Error" : "None";
    },

    formatValidationText: function (bShown, bMissing) {
      if (!(bShown && bMissing)) {
        return "";
      }
      return this.getResourceBundle().getText("requiredFieldHint");
    },

    _getValidationRules: function () {
      var oState = this.getModel("state");
      return {
        requiredFields: oState.getProperty("/requiredFields") || []
      };
    },

    _isBarrierAllowedByLpc: function () {
      var sLpcKey = this.getModel("selected").getProperty("/basic/LPC_KEY");
      return ChecklistValidationService.isBarrierSectionAllowed(sLpcKey);
    },

    formatBooleanResultText: function (vValue) {
      if (vValue === true) {
        return this.getResourceBundle().getText("statusOk");
      }
      if (vValue === false) {
        return this.getResourceBundle().getText("statusFailed");
      }
      return "-";
    },

    formatBooleanResultState: function (vValue) {
      if (vValue === true) {
        return "Success";
      }
      if (vValue === false) {
        return "Error";
      }
      return "None";
    },

    formatLifecycleStatusText: function (sStatus) {
      var oBundle = this.getResourceBundle();
      switch (String(sStatus || "").toUpperCase()) {
        case "DRAFT": return oBundle.getText("statusDraft");
        case "REGISTERED": return oBundle.getText("statusRegistered");
        case "CLOSED": return oBundle.getText("statusClosed");
        default: return sStatus || "-";
      }
    },

    formatLifecycleStatusState: function (sStatus) {
      switch (String(sStatus || "").toUpperCase()) {
        case "REGISTERED": return "Warning";
        case "CLOSED": return "Success";
        default: return "Information";
      }
    },

    formatPassedTotal: function (aRows) {
      var a = aRows || [];
      var iTotal = a.length;
      var iPassed = a.filter(function (oRow) { return !!(oRow && oRow.result); }).length;
      return iPassed + "/" + iTotal;
    },

    isBarriersVisibleByLpc: function (sLpcKey) {
      return ChecklistValidationService.isBarrierSectionAllowed(sLpcKey);
    },

    _updateDerivedRootResult: function () {
      var oSelected = this.getModel("selected").getData() || {};
      var vOverall = ChecklistValidationService.buildOverallResult(oSelected);
      this.getModel("selected").setProperty("/root/overall_result", vOverall);
    },

    _runChecklistValidation: function () {
      var oSelected = this.getModel("selected").getData() || {};
      return ChecklistValidationService.validateForStatusChange(oSelected, this._getValidationRules());
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
        this._updateDerivedRootResult();
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
        this._updateDerivedRootResult();
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
        return BackendAdapter.getChecklistRoot(sId).then((oRootOnly) => {
          oDataModel.setProperty("/selectedChecklist", oRootOnly || null);
          this._applyChecklistLazily(oRootOnly || {});
          return oRootOnly;
        });
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
      var sNeedle = String(sId || "").trim();
      var oLocalMatch = aChecklists.find(function (item) {
        return String((((item || {}).root || {}).id) || "").trim() === sNeedle;
      }) || null;

      if (oLocalMatch) {
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
        this._updateDerivedRootResult();
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
        this._updateDerivedRootResult();
      }.bind(this));

      this._loadSectionPaged(sId, "barriers", iPageSize).then(function (aBarriers) {
        this.getModel("selected").setProperty("/barriers", aBarriers || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/barriersLoading", false);
        if (!oView.getProperty("/checksLoading")) {
          oView.setProperty("/detailSkeletonBusy", false);
        }
        this._updateSelectionState();
        this._updateDerivedRootResult();
      }.bind(this));
    },

    onPersonSuggest: function (oEvent) {
      var sValue = this._normalizeText(oEvent.getParameter("suggestValue"));
      var sTarget = oEvent.getSource().data("target");
      var aPersons = this.getModel("masterData").getProperty("/persons") || [];
      var aFiltered = aPersons.filter((oPerson) => {
        var sName = this._normalizeText(oPerson.fullName);
        var sPernr = this._normalizeText(oPerson.perner);
        var sPosition = this._normalizeText(oPerson.position);
        return !sValue || sName.indexOf(sValue) >= 0 || sPernr.indexOf(sValue) >= 0 || sPosition.indexOf(sValue) >= 0;
      }).slice(0, 10);

      this._setPersonSuggestions(sTarget, aFiltered);

      if (sValue && aFiltered.length < 3 && BackendAdapter.suggestPersons) {
        BackendAdapter.suggestPersons(sValue).then((aRemote) => {
          this._setPersonSuggestions(sTarget, (aRemote || []).slice(0, 10));
        }).catch(function () {
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
      this._setBasicTextByDictionary(oEvent, "/lpc", "/basic/LPC_KEY", "/basic/LPC_TEXT");
      this._syncSelectionMeta();

      var bAllowBarriers = this._isBarrierAllowedByLpc();
      if (bAllowBarriers) {
        return;
      }

      var aBarriers = this.getModel("selected").getProperty("/barriers") || [];
      if (!aBarriers.length) {
        return;
      }

      MessageBox.warning(this.getResourceBundle().getText("barriersWillBeRemovedPrompt"), {
        actions: [MessageBox.Action.YES, MessageBox.Action.NO],
        emphasizedAction: MessageBox.Action.NO,
        onClose: function (sAction) {
          if (sAction === MessageBox.Action.YES) {
            this.getModel("selected").setProperty("/barriers", []);
            this._syncSelectionMeta();
            return;
          }

          var oSelectedModel = this.getModel("selected");
          oSelectedModel.setProperty("/basic/LPC_KEY", "");
          oSelectedModel.setProperty("/basic/LPC_TEXT", "");
          this._syncSelectionMeta();
        }.bind(this)
      });
    },

    onProfessionChange: function (oEvent) {
      this._setBasicTextByDictionary(oEvent, "/professions", "/basic/PROF_KEY", "/basic/PROF_TEXT");
      this._syncSelectionMeta();
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
      var fnProceed = () => {
        var sObjectId = oState.getProperty("/activeObjectId");
        var sSessionId = oState.getProperty("/sessionId");
        this.releaseLock(sObjectId, sSessionId);
        oState.setProperty("/layout", "OneColumn");
        oState.setProperty("/activeObjectId", null);
        oState.setProperty("/isDirty", false);
        this.navTo("search", {}, true);
      };

      if (!oState.getProperty("/isDirty")) {
        fnProceed();
        return;
      }

      FlowCoordinator.confirmUnsavedAndHandle(this, this.onSaveDetail.bind(this)).then((sDecision) => {
        if (sDecision === "DISCARD" || sDecision === "SAVE") {
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

      var fnDisableEditAndRelease = () => this._releaseEditLock(sObjectId, sSessionId, oStateModel);

      if (!bEditMode && oStateModel.getProperty("/isDirty")) {
        FlowCoordinator.confirmUnsavedAndHandle(this, this.onSaveDetail.bind(this)).then((sDecision) => {
          if (sDecision === "CANCEL") {
            return;
          }
          this.setLockUiState(oStateModel, "PENDING", this.getResourceBundle().getText("lockPending"));
          this.runWithStateFlag(oStateModel, "/lockOperationPending", fnDisableEditAndRelease);
        });
        return;
      }

      this.setLockUiState(oStateModel, "PENDING", this.getResourceBundle().getText("lockPending"));
      this.runWithStateFlag(oStateModel, "/lockOperationPending", function () {
        if (!bEditMode) {
          return fnDisableEditAndRelease();
        }

        return this._ensureChecklistFreshBeforeEdit(sObjectId).then(() => {
          return this._confirmIntegrationEdit();
        }).then((bConfirmed) => {
          if (!bConfirmed) {
            this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockStayReadOnly"));
            return null;
          }
          return BackendAdapter.lockAcquire(
            sObjectId,
            sSessionId
          );
        }).then((oAcquireResult) => {
          if (!oAcquireResult) {
            return;
          }
          oStateModel.setProperty("/isLocked", true);
          oStateModel.setProperty("/mode", "EDIT");
          this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        }).catch((oError) => {
          return this._tryStealOwnLock(sObjectId, sSessionId, oError).catch(() => {
            oStateModel.setProperty("/mode", "READ");
            oStateModel.setProperty("/isLocked", false);
            this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockOwnedByOther"));
            return FlowCoordinator.handleBackendError(this, oError);
          });
        });
      }.bind(this));
    },

    onCancelEditFromDetail: function () {
      var oSelected = this.getModel("data").getProperty("/selectedChecklist") || {};
      var oStateModel = this.getModel("state");
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      this.getModel("selected").setData(ChecklistDraftHelper.clone(oSelected));
      oStateModel.setProperty("/isDirty", false);
      this._releaseEditLock(sObjectId, sSessionId, oStateModel);
    },

    _updateSelectionState: function () {
      var oSelectedModel = this.getModel("selected");
      var oViewModel = this.getView().getModel("view");
      var aChecks = oSelectedModel.getProperty("/checks") || [];
      var aBarriers = oSelectedModel.getProperty("/barriers") || [];

      if (!oViewModel) {
        return;
      }

      oViewModel.setProperty("/hasSelectedChecks", aChecks.some((oItem) => !!(oItem && oItem.selected)));
      oViewModel.setProperty("/hasSelectedBarriers", aBarriers.some((oItem) => !!(oItem && oItem.selected)));
    },

    onDeleteSelectedChecks: function () {
      this._mutateRows("/checks", RowListHelper.removeSelectedRows);
    },

    onDeleteSelectedBarriers: function () {
      this._mutateRows("/barriers", RowListHelper.removeSelectedRows);
    },

    onSelectionToggle: function () {
      this._syncSelectionMeta();
    },

    formatInfoCardValue: function (sKey, sDate, sTime, sTimezone, sEquipment, sObserver, sObserved, sLocationName, sLocationText, sLpcText, sProfText) {
      if (sKey === "date") {
        return sDate || "-";
      }

      if (sKey === "time") {
        return sTime || "-";
      }

      if (sKey === "timezone") {
        return sTimezone || "-";
      }

      if (sKey === "equipment") {
        return sEquipment || "-";
      }

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

    onValidateChecklist: function () {
      var oValidation = this._runChecklistValidation();
      var oViewModel = this.getView().getModel("view");
      if (oViewModel) {
        oViewModel.setProperty("/validationShown", !oValidation.valid);
        oViewModel.setProperty("/validationMissing", ChecklistUiState.buildValidationMap(oValidation.missingPaths));
      }

      if (oValidation.valid) {
        this.showI18nToast("checklistValidationPassed");
        return Promise.resolve(true);
      }

      MessageBox.warning(this.getResourceBundle().getText("checklistValidationFailed", [oValidation.missingPaths.length + (oValidation.hasAtLeastOneCheck ? 0 : 1)]));
      return Promise.resolve(false);
    },

    _applyStatusAndSave: function (sTargetStatus) {
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/root/status", ChecklistUiState.normalizeStatus(sTargetStatus));
      this.getModel("state").setProperty("/isDirty", true);
      return this.onSaveDetail().then(function (oResult) {
        this.showI18nToast("statusChanged");
        return oResult;
      }.bind(this));
    },

    onChangeChecklistStatus: function (oEvent) {
      var sStatus = oEvent.getSource().data("targetStatus") || "";
      if (!sStatus) {
        return;
      }

      return this.onValidateChecklist().then(function (bValid) {
        if (!bValid) {
          return;
        }

        var oSelected = this.getModel("selected").getData() || {};

        if (ChecklistUiState.isSameStatus((oSelected.root || {}).status, sStatus)) {
          return;
        }

        if ((oSelected.root || {}).this_is_integration_data) {
          return this._confirmIntegrationEdit().then(function (bConfirmed) {
            if (!bConfirmed) {
              return;
            }
            return this._applyStatusAndSave(sStatus);
          }.bind(this));
        }

        return this._applyStatusAndSave(sStatus);
      }.bind(this));
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
      var oEdited = this.getModel("selected").getData() || {};
      var sId = ((((oEdited || {}).root || {}).id) || "").trim();

      var bCreateMode = !sId;

      return this.runWithStateFlag(oStateModel, "/isLoading", function () {

      return (bCreateMode ? BackendAdapter.createCheckList(oEdited) : BackendAdapter.updateCheckList(sId, oEdited))
        .then((oSavedChecklist) => {
          return BackendAdapter.getCheckLists().then((aUpdatedCheckLists) => {
            return {
              savedChecklist: oSavedChecklist,
              checkLists: aUpdatedCheckLists
            };
          });
        })
        .then((oResult) => {
          oDataModel.setProperty("/checkLists", oResult.checkLists);
          oDataModel.setProperty("/visibleCheckLists", oResult.checkLists);
          oDataModel.setProperty("/selectedChecklist", oResult.savedChecklist);
          this.getModel("selected").setData(oResult.savedChecklist);
          if (oStateModel.getProperty("/isLocked")) {
            oStateModel.setProperty("/mode", "EDIT");
          }
          oStateModel.setProperty("/isDirty", false);
          window.dispatchEvent(new CustomEvent("pcct:fullSave"));
          this.showI18nToast("objectSaved");
        })
        .catch((oError) => {
          return FlowCoordinator.handleBackendError(this, oError, {
            onConflictChoice: (sChoice) => {
              if (sChoice === this.getResourceBundle().getText("reloadButton")) {
                return BackendAdapter.getChecklistRoot(sId).then((oRootOnly) => {
                  if (oRootOnly) {
                    this.getModel("selected").setData(ChecklistDraftHelper.clone(oRootOnly));
                    this._applyChecklistLazily(oRootOnly);
                  }
                });
              }
              if (sChoice === this.getResourceBundle().getText("overwriteButton")) {
                return BackendAdapter.updateCheckList(sId, oEdited, { force: true });
              }
              return null;
            }
          });
        });
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
      this._closeExpandedDialogById("checksExpandedDialog");
    },

    onCloseBarriersExpanded: function () {
      this._closeExpandedDialogById("barriersExpandedDialog");
    },

    onAddCheckRow: function () {
      this._mutateRows("/checks", RowListHelper.addRow);
    },

    onAddBarrierRow: function () {
      this._mutateRows("/barriers", RowListHelper.addRow);
    },

    onDeleteCheckRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "selected", "/checks");
      if (!oResult.deleted) {
        return;
      }
      this._syncSelectionMeta();
    },

    onDeleteBarrierRow: function (oEvent) {
      var oResult = this.deleteRowFromEvent(oEvent, "selected", "/barriers");
      if (!oResult.deleted) {
        return;
      }
      this._syncSelectionMeta();
    },

    onExit: function () {
      ["checksExpandedDialog", "barriersExpandedDialog"].forEach((sId) => {
        var oDialog = this.byId(sId);
        if (oDialog) {
          oDialog.destroy();
        }
      });
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
