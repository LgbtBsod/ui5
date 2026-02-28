sap.ui.define([
  "sap_ui5/controller/Base.controller",
  "sap_ui5/service/backend/BackendAdapter",
  "sap/m/MessageToast",
  "sap/m/MessageBox",
  "sap/ui/model/json/JSONModel",
  "sap_ui5/util/RowListHelper",
  "sap_ui5/util/ChecklistDraftHelper",
  "sap_ui5/util/FlowCoordinator",
  "sap_ui5/util/ChecklistValidationService",
  "sap_ui5/util/ChecklistUiState",
  "sap_ui5/util/DetailCardSchema",
  "sap_ui5/util/DetailFormatters",
  "sap_ui5/service/usecase/ChecklistCrudUseCase",
  "sap_ui5/service/usecase/DetailLifecycleUseCase",
  "sap_ui5/service/usecase/DetailStatusRowUseCase",
  "sap_ui5/service/usecase/DetailStatusCommandUseCase",
  "sap_ui5/service/usecase/DetailExpandedRowsFlowUseCase",
  "sap_ui5/service/usecase/DetailDialogLifecycleUseCase",
  "sap_ui5/service/usecase/DetailLockReleaseUseCase",
  "sap_ui5/service/usecase/DetailSaveSuccessFlowUseCase",
  "sap_ui5/service/usecase/DetailToolbarValidationUseCase",
  "sap_ui5/service/usecase/DetailSaveErrorPresentationUseCase",
  "sap_ui5/service/usecase/DetailSaveErrorOutcomePresentationUseCase",
  "sap_ui5/service/usecase/DetailLocationValueHelpUseCase",
  "sap_ui5/service/usecase/DetailPersonSuggestionUseCase",
  "sap_ui5/service/usecase/DetailDictionarySelectionUseCase",
  "sap_ui5/service/usecase/DetailLpcBarrierWarningFlowUseCase",
  "sap_ui5/service/usecase/DetailIntegrationEditWarningUseCase",
  "sap_ui5/service/usecase/OperationalKpiInstrumentationUseCase",
  "sap_ui5/service/usecase/DetailCloseFlowOrchestrationUseCase",
  "sap_ui5/service/usecase/DetailToggleEditOrchestrationUseCase",
  "sap_ui5/service/usecase/DetailSaveFlowOrchestrationUseCase",
  "sap_ui5/service/usecase/DetailSelectionMetaSyncUseCase",
  "sap_ui5/util/UxTelemetry"
], function (BaseController, BackendAdapter, MessageToast, MessageBox, JSONModel, RowListHelper, ChecklistDraftHelper, FlowCoordinator, ChecklistValidationService, ChecklistUiState, DetailCardSchema, DetailFormatters, ChecklistCrudUseCase, DetailLifecycleUseCase, DetailStatusRowUseCase, DetailStatusCommandUseCase, DetailExpandedRowsFlowUseCase, DetailDialogLifecycleUseCase, DetailLockReleaseUseCase, DetailSaveSuccessFlowUseCase, DetailToolbarValidationUseCase, DetailSaveErrorPresentationUseCase, DetailSaveErrorOutcomePresentationUseCase, DetailLocationValueHelpUseCase, DetailPersonSuggestionUseCase, DetailDictionarySelectionUseCase, DetailLpcBarrierWarningFlowUseCase, DetailIntegrationEditWarningUseCase, OperationalKpiInstrumentationUseCase, DetailCloseFlowOrchestrationUseCase, DetailToggleEditOrchestrationUseCase, DetailSaveFlowOrchestrationUseCase, DetailSelectionMetaSyncUseCase, UxTelemetry) {
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
          DetailLifecycleUseCase.setReadUnlocked(oStateModel);
          this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockStayReadOnly"));
          return null;
        }

        DetailLifecycleUseCase.setEditLocked(oStateModel);
        this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        return oResult;
      });
    },

    onInit: function () {
      var oBundle = this.getResourceBundle();
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
        statusActions: DetailCardSchema.createStatusActions(oBundle),
        infoCards: DetailCardSchema.createInfoCards(oBundle),
        locationVhTree: [],
        locationVhSelectedNode: null,
        locationVhHasSelection: false,
        detailControlCollapsed: false
      });

      this.getView().setModel(oViewModel, "view");
      this.attachRouteMatched("detail", this._onMatched);
      this.attachRouteMatched("detailLayout", this._onMatched);
      this.getModel("selected").attachPropertyChange(this._onSelectedChanged, this);
      this.getModel("mpl").attachPropertyChange(this._onMplChanged, this);
    },

    _onMplChanged: function (oEvent) {
      if ((oEvent.getParameter("path") || "") !== "/locations") {
        return;
      }
      if ((this.getModel("mpl").getProperty("/locations") || []).length) {
        this._prepareLocationTree();
      }
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
      return DetailSelectionMetaSyncUseCase.runSelectionMetaSync({
        syncDirty: this._syncDirtyFlag.bind(this),
        updateSelectionState: this._updateSelectionState.bind(this),
        updateDerivedRootResult: this._updateDerivedRootResult.bind(this)
      });
    },

    _releaseEditLock: function (sObjectId, sSessionId, oStateModel) {
      return DetailLockReleaseUseCase.runReleaseFlow({
        stateModel: oStateModel,
        releaseLock: function () {
          return this.releaseLock(sObjectId, sSessionId, { trySave: false });
        }.bind(this),
        setLockUiIdle: function () {
          this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockReleased"));
        }.bind(this)
      });
    },

    _mutateRows: function (sPath, fnMutator) {
      DetailExpandedRowsFlowUseCase.mutateRows({
        selectedModel: this.getModel("selected"),
        path: sPath,
        mutator: fnMutator,
        onMutated: this._syncSelectionMeta.bind(this)
      });
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
      return DetailFormatters.validationState(bShown, bMissing);
    },

    formatValidationText: function (bShown, bMissing) {
      return DetailFormatters.validationText(bShown, bMissing, this.getResourceBundle());
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
      return DetailFormatters.booleanResultText(vValue, this.getResourceBundle());
    },

    formatBooleanResultState: function (vValue) {
      return DetailFormatters.booleanResultState(vValue);
    },

    formatLifecycleStatusText: function (sStatus) {
      return DetailFormatters.lifecycleStatusText(sStatus, this.getResourceBundle());
    },

    formatDraftStateText: function (bDirty) {
      return DetailFormatters.draftStateText(bDirty, this.getResourceBundle());
    },

    formatDraftStateState: function (bDirty) {
      return DetailFormatters.draftStateState(bDirty);
    },

    formatLockOperationText: function (sOperationText, sMode) {
      return DetailFormatters.lockOperationText(sOperationText, sMode, this.getResourceBundle());
    },

    formatLockOperationState: function (sOperationState, sMode) {
      return DetailFormatters.lockOperationState(sOperationState, sMode);
    },

    formatLifecycleStatusState: function (sStatus) {
      return DetailFormatters.lifecycleStatusState(sStatus);
    },

    formatPassedTotal: function (aRows) {
      return DetailFormatters.passedTotal(aRows);
    },

    formatHeaderDate: function (sDate) {
      return DetailStatusRowUseCase.formatHumanDateLong(sDate);
    },

    hasRows: function (aRows) {
      return Array.isArray(aRows) && aRows.length > 0;
    },

    isBarriersRatioVisible: function (sLpcKey, aRows) {
      return ChecklistValidationService.isBarrierSectionAllowed(sLpcKey) && Array.isArray(aRows) && aRows.length > 0;
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

    _buildFrontendNowPayload: function () {
      var oNow = new Date();
      var sDate = oNow.toISOString().slice(0, 10);
      var sTime = String(oNow.getHours()).padStart(2, "0") + ":" + String(oNow.getMinutes()).padStart(2, "0");
      var sTimezone = Intl.DateTimeFormat().resolvedOptions().timeZone || "UTC";
      return { date: sDate, time: sTime, timezone: sTimezone };
    },

    _applyFrontendNowDefaults: function (oChecklist, bForce) {
      if (!oChecklist) {
        return oChecklist;
      }
      oChecklist.basic = oChecklist.basic || {};
      var oNow = this._buildFrontendNowPayload();
      if (bForce || !oChecklist.basic.date) {
        oChecklist.basic.date = oNow.date;
      }
      if (bForce || !oChecklist.basic.time) {
        oChecklist.basic.time = oNow.time;
      }
      if (bForce || !oChecklist.basic.timezone) {
        oChecklist.basic.timezone = oNow.timezone;
      }
      return oChecklist;
    },

    _onMatched: function (oEvent) {
      var sId = oEvent.getParameter("arguments").id;
      var oStateModel = this.getModel("state");
      var sLayout = oEvent.getParameter("arguments").layout || oStateModel.getProperty("/preferredDetailLayout") || "TwoColumnsMidExpanded";
      var oDataModel = this.getModel("data");
      var sAction = oStateModel.getProperty("/objectAction") || "";
      var bCreate = sAction === "CREATE" || sId === "__create";
      var bCopy = sAction === "COPY";

      oStateModel.setProperty("/layout", sLayout);
      oStateModel.setProperty("/preferredDetailLayout", sLayout);
      oStateModel.setProperty("/activeObjectId", bCreate ? null : (sId || null));
      oStateModel.setProperty("/objectAction", "");
      oStateModel.setProperty("/copySourceId", bCopy ? (sId || null) : null);

      this._prepareLocationTree();

      if (bCreate) {
        // Single-card flow: create is handled on detail card directly.
        var oDraft = ChecklistDraftHelper.buildDefaultChecklist("");
        this._applyFrontendNowDefaults(oDraft, true);
        oDataModel.setProperty("/selectedChecklist", oDraft);
        this.getModel("selected").setData(ChecklistDraftHelper.clone(oDraft));
        oStateModel.setProperty("/mode", "EDIT");
        DetailLifecycleUseCase.resetDirty(oStateModel);
        this._syncSelectionMeta();
        return;
      }

      if (bCopy) {
        var oSeed = this.getModel("selected").getData() || oDataModel.getProperty("/selectedChecklist") || {};
        var oCopy = ChecklistDraftHelper.clone(oSeed);
        this._applyFrontendNowDefaults(oCopy, true);
        if (!oCopy.root) { oCopy.root = {}; }
        oCopy.root.id = "";
        oDataModel.setProperty("/selectedChecklist", oCopy);
        this.getModel("selected").setData(ChecklistDraftHelper.clone(oCopy));
        oStateModel.setProperty("/activeObjectId", null);
        oStateModel.setProperty("/mode", "EDIT");
        oStateModel.setProperty("/isDirty", true);
        this._syncSelectionMeta();
        return;
      }

      oStateModel.setProperty("/mode", "READ");
      this._bindChecklistById(sId);
      oStateModel.setProperty("/isDirty", false);
    },

    _ensureMplLocationsLoaded: function () {
      var oMplModel = this.getModel("mpl");
      var oState = this.getModel("state");
      var aExisting = oMplModel.getProperty("/locations") || [];

      if (Array.isArray(aExisting) && aExisting.length) {
        this._prepareLocationTree();
        return Promise.resolve(aExisting);
      }

      oState.setProperty("/locationsLoading", true);
      return BackendAdapter.getLocations().then(function (aRemote) {
        var aLocations = Array.isArray(aRemote) ? aRemote : [];
        if (aLocations.length) {
          oMplModel.setProperty("/locations", aLocations);
          this._prepareLocationTree();
        }
        return aLocations;
      }.bind(this)).catch(function () {
        return [];
      }).finally(function () {
        oState.setProperty("/locationsLoading", false);
      });
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

    _isChecklistCacheStrictFresh: function () {
      var oComp = this.getOwnerComponent();
      return !!(oComp && oComp._oSmartCache && oComp._oSmartCache.isCacheStrictFresh && oComp._oSmartCache.isCacheStrictFresh("checkLists"));
    },

    _reloadChecklistFromBackend: function (sId) {
      var oDataModel = this.getModel("data");
      return this.runWithStateFlag(this.getModel("state"), "/isLoading", function () {
        return ChecklistCrudUseCase.getChecklistRoot(sId).then((oRootOnly) => {
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

      if (this._isChecklistCacheStrictFresh()) {
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
        this._syncSelectionMeta();
        return;
      }

      this._loadSectionPaged(sId, "checks", iPageSize).then(function (aChecks) {
        this.getModel("selected").setProperty("/checks", aChecks || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/checksLoading", false);
        if (!oView.getProperty("/barriersLoading")) {
          oView.setProperty("/detailSkeletonBusy", false);
        }
        this._syncSelectionMeta();
      }.bind(this));

      this._loadSectionPaged(sId, "barriers", iPageSize).then(function (aBarriers) {
        this.getModel("selected").setProperty("/barriers", aBarriers || []);
      }.bind(this)).finally(function () {
        oView.setProperty("/barriersLoading", false);
        if (!oView.getProperty("/checksLoading")) {
          oView.setProperty("/detailSkeletonBusy", false);
        }
        this._syncSelectionMeta();
      }.bind(this));
    },

    onPersonSuggest: function (oEvent) {
      var sQueryRaw = oEvent.getParameter("suggestValue") || "";
      var sQuery = this._normalizeText(sQueryRaw);
      var sTarget = oEvent.getSource().data("target");
      var aPersons = this.getModel("masterData").getProperty("/persons") || [];
      var aFiltered = DetailPersonSuggestionUseCase.filterSuggestions({
        query: sQueryRaw,
        persons: aPersons,
        normalizeText: this._normalizeText.bind(this),
        limit: 10
      });

      this._setPersonSuggestions(sTarget, aFiltered);

      if (BackendAdapter.suggestPersons
        && DetailPersonSuggestionUseCase.shouldFetchRemoteSuggestions({
          query: sQuery,
          localCount: aFiltered.length,
          minLocalBeforeRemote: 3
        })) {
        BackendAdapter.suggestPersons(sQuery).then(function (aRemote) {
          var aMerged = DetailPersonSuggestionUseCase.dedupeSuggestions((aRemote || []).concat(aFiltered || [])).slice(0, 10);
          this._setPersonSuggestions(sTarget, aMerged);
        }.bind(this)).catch(function () {
          // Keep local suggestions only when remote suggest is unavailable.
        });
      }
    },

    _setPersonSuggestions: function (sTarget, aSuggestions) {
      DetailPersonSuggestionUseCase.applySuggestionsToViewModel({
        target: sTarget,
        suggestions: aSuggestions,
        viewModel: this.getView().getModel("view")
      });
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
      var oResolved = DetailPersonSuggestionUseCase.resolvePersonFromSuggestionEvent(oEvent);
      if (!oResolved.person) {
        return;
      }

      DetailPersonSuggestionUseCase.applyPersonSelection({
        target: oResolved.target,
        person: oResolved.person,
        selectedModel: this.getModel("selected")
      });
    },

    formatHeartbeatText: function (sMode, bIsLocked) {
      if (sMode === "EDIT" && bIsLocked) {
        return this.getResourceBundle().getText("heartbeatLockedActive");
      }
      return this.getResourceBundle().getText("heartbeatInactive");
    },

    formatAutosaveAt: function (sAutosaveAt) {
      if (!sAutosaveAt) {
        return this.getResourceBundle().getText("autosaveLastSyncNever");
      }
      return this.getResourceBundle().getText("autosaveLastSyncAt", [new Date(sAutosaveAt).toLocaleTimeString()]);
    },

    formatAutosaveText: function (sMode, sAutosaveState) {
      if (sMode !== "EDIT") {
        return this.getResourceBundle().getText("autosaveDisabled");
      }
      if (sAutosaveState === "ERROR") {
        return this.getResourceBundle().getText("autosaveError");
      }
      if (sAutosaveState === "SAVING") {
        return this.getResourceBundle().getText("autosaveSaving");
      }
      if (sAutosaveState === "SAVED") {
        return this.getResourceBundle().getText("autosaveSaved");
      }
      return this.getResourceBundle().getText("autosaveWaiting");
    },

    onToggleDetailControlRail: function () {
      var oViewModel = this.getView().getModel("view");
      var bCollapsed = !!oViewModel.getProperty("/detailControlCollapsed");
      oViewModel.setProperty("/detailControlCollapsed", !bCollapsed);
    },

    onLocationSelectionChange: function (oEvent) {
      return DetailLocationValueHelpUseCase.runListSelectionLifecycle({
        event: oEvent,
        selectedModel: this.getModel("selected")
      });
    },

    onOpenLocationValueHelp: function () {
      this.getView().getModel("view").setProperty("/locationVhSelectedNode", null);
      this.getView().getModel("view").setProperty("/locationVhHasSelection", false);
      return DetailLocationValueHelpUseCase.runOpenValueHelpLifecycle({
        dialog: this.byId("locationValueHelpDialog"),
        locations: this.getModel("mpl").getProperty("/locations") || [],
        viewModel: this.getView().getModel("view"),
        buildLocationTree: this._buildLocationTree.bind(this),
        normalizeText: this._normalizeText.bind(this),
        ensureLocationsLoaded: this._ensureMplLocationsLoaded.bind(this),
        table: this.byId("locationValueHelpTreeTable")
      });
    },

    onCloseLocationValueHelp: function () {
      var oViewModel = this.getView().getModel("view");
      oViewModel.setProperty("/locationVhSelectedNode", null);
      oViewModel.setProperty("/locationVhHasSelection", false);
      DetailLocationValueHelpUseCase.closeValueHelp({
        dialog: this.byId("locationValueHelpDialog")
      });
    },

    onLocationValueHelpSearch: function (oEvent) {
      DetailLocationValueHelpUseCase.applyFilteredTreeToViewModel({
        query: oEvent.getParameter("newValue"),
        locations: this.getModel("mpl").getProperty("/locations") || [],
        viewModel: this.getView().getModel("view"),
        buildLocationTree: this._buildLocationTree.bind(this),
        normalizeText: this._normalizeText.bind(this)
      });
    },


    _applyLocationSelection: function (oNode) {
      DetailLocationValueHelpUseCase.applyLocationSelection({
        node: oNode,
        selectedModel: this.getModel("selected")
      });
    },

    onLocationTreeSelectionChange: function (oEvent) {
      var oNode = DetailLocationValueHelpUseCase.resolveNodeFromTreeSelectionEvent(oEvent);
      var oViewModel = this.getView().getModel("view");
      oViewModel.setProperty("/locationVhSelectedNode", oNode || null);
      oViewModel.setProperty("/locationVhHasSelection", !!oNode);
      return { ok: !!oNode, reason: oNode ? "node_selected" : "missing_node" };
    },

    onConfirmLocationValueHelp: function () {
      var oViewModel = this.getView().getModel("view");
      var oNode = oViewModel.getProperty("/locationVhSelectedNode");
      if (!oNode) {
        return { ok: false, reason: "missing_node" };
      }
      this._applyLocationSelection(oNode);
      this.onCloseLocationValueHelp();
      return { ok: true, reason: "selection_applied" };
    },

    onLocationComboChange: function (oEvent) {
      return DetailLocationValueHelpUseCase.runComboSelectionLifecycle({
        event: oEvent,
        modelName: "mpl",
        selectedModel: this.getModel("selected")
      });
    },

    onLpcChange: function (oEvent) {
      return DetailDictionarySelectionUseCase.runLpcSelectionLifecycle({
        event: oEvent,
        dictionary: this.getModel("masterData").getProperty("/lpc") || [],
        keyPath: "/basic/LPC_KEY",
        textPath: "/basic/LPC_TEXT",
        selectedModel: this.getModel("selected"),
        onAfterApply: this._syncSelectionMeta.bind(this),
        openWarningDialog: DetailLpcBarrierWarningFlowUseCase.openWarningDialog,
        messageBox: MessageBox,
        promptText: this.getResourceBundle().getText("barriersWillBeRemovedPrompt"),
        barrierAllowed: this._isBarrierAllowedByLpc(),
        barriers: this.getModel("selected").getProperty("/barriers") || []
      });
    },

    onProfessionChange: function (oEvent) {
      return DetailDictionarySelectionUseCase.runProfessionSelectionLifecycle({
        event: oEvent,
        dictionary: this.getModel("masterData").getProperty("/professions") || [],
        keyPath: "/basic/PROF_KEY",
        textPath: "/basic/PROF_TEXT",
        selectedModel: this.getModel("selected"),
        onAfterApply: this._syncSelectionMeta.bind(this)
      });
    },

    _confirmIntegrationEdit: function () {
      return DetailIntegrationEditWarningUseCase.confirmIntegrationEdit({
        selectedRoot: this.getModel("selected").getProperty("/root"),
        dataRoot: this.getModel("data").getProperty("/selectedChecklist/root"),
        messageBox: MessageBox,
        bundle: this.getResourceBundle()
      });
    },

    onCloseDetail: function () {
      var oState = this.getModel("state");
      return DetailCloseFlowOrchestrationUseCase.runCloseFlow({
        stateModel: oState,
        host: this,
        onSave: this.onSaveDetail.bind(this),
        releaseLock: this.releaseLock.bind(this),
        navigateToSearch: function () {
          this.navTo("search", {}, true);
        }.bind(this)
      });
    },

    onToggleEdit: function (oEvent) {
      return this.onToggleEditFromDetail(oEvent);
    },

    onToggleEditFromDetail: function (oEvent) {
      this._syncDirtyFlag();
      var bEditMode = oEvent.getParameter("state");
      var oSource = oEvent.getSource && oEvent.getSource();
      var oStateModel = this.getModel("state");
      if (oSource && typeof oSource.setState === "function") {
        oSource.setState(oStateModel.getProperty("/mode") === "EDIT");
      }
      if (oStateModel.getProperty("/lockOperationPending")) {
        if (oSource && typeof oSource.setState === "function") {
          oSource.setState(oStateModel.getProperty("/mode") === "EDIT");
        }
        return Promise.resolve({ ok: false, reason: "pending_lock_operation" });
      }

      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");
      var fnDisableEditAndRelease = () => this._releaseEditLock(sObjectId, sSessionId, oStateModel);
      var oBundle = this.getResourceBundle();

      return DetailToggleEditOrchestrationUseCase.runToggleFlow({
        host: this,
        stateModel: oStateModel,
        editMode: bEditMode,
        onSave: this.onSaveDetail.bind(this),
        pendingText: oBundle.getText("lockPending"),
        stayReadOnlyText: oBundle.getText("lockStayReadOnly"),
        lockOwnedText: oBundle.getText("lockOwnedByMe"),
        lockOwnedByOtherText: oBundle.getText("lockOwnedByOther"),
        setLockUiState: function (sState, sText) {
          this.setLockUiState(oStateModel, sState, sText);
        }.bind(this),
        runWithStateFlag: function (sPath, fnTask) {
          return this.runWithStateFlag(oStateModel, sPath, fnTask);
        }.bind(this),
        releaseEdit: fnDisableEditAndRelease,
        ensureFreshBeforeEdit: function () {
          if (!sObjectId) {
            return Promise.resolve();
          }
          return this._ensureChecklistFreshBeforeEdit(sObjectId);
        }.bind(this),
        confirmIntegrationEdit: this._confirmIntegrationEdit.bind(this),
        acquireLock: function () {
          return BackendAdapter.lockAcquire(sObjectId, sSessionId, this._getCurrentUser());
        }.bind(this),
        tryRecoverFromAcquireError: function (oError) {
          return this._tryStealOwnLock(sObjectId, sSessionId, oError).then(function (oResult) {
            return !!(oResult && oResult.success);
          }).catch(function () {
            return false;
          });
        }.bind(this),
        onAcquireFailed: function (oError) {
          return FlowCoordinator.handleBackendError(this, oError);
        }.bind(this)
      }).finally(function () {
        if (oSource && typeof oSource.setState === "function") {
          oSource.setState(oStateModel.getProperty("/mode") === "EDIT");
        }
      });
    },

    onCancelEditFromDetail: function () {
      this._syncDirtyFlag();
      var oStateModel = this.getModel("state");
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      return DetailToggleEditOrchestrationUseCase.runCancelEditFlow({
        sourceChecklist: this.getModel("data").getProperty("/selectedChecklist") || {},
        cloneChecklist: ChecklistDraftHelper.clone,
        selectedModel: this.getModel("selected"),
        stateModel: oStateModel,
        releaseEdit: this._releaseEditLock.bind(this, sObjectId, sSessionId, oStateModel)
      });
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
      return DetailStatusRowUseCase.formatInfoCardValue(sKey, {
        date: sDate,
        time: sTime,
        timezone: sTimezone,
        equipment: sEquipment,
        observer: sObserver,
        observed: sObserved,
        locationName: sLocationName,
        locationText: sLocationText,
        lpcText: sLpcText,
        profText: sProfText
      });
    },

    onValidateChecklist: function () {
      var oValidation = this._runChecklistValidation();
      var oViewModel = this.getView().getModel("view");

      DetailToolbarValidationUseCase.applyValidationState(oViewModel, oValidation, ChecklistUiState.buildValidationMap);

      if (oValidation.valid) {
        this.showI18nToast("checklistValidationPassed");
        return Promise.resolve(true);
      }

      MessageBox.warning(this.getResourceBundle().getText("checklistValidationFailed", [
        DetailToolbarValidationUseCase.resolveValidationWarningCount(oValidation)
      ]));
      OperationalKpiInstrumentationUseCase.markValidationFailure(this.getModel("state"));
      return Promise.resolve(false);
    },

    _applyStatusAndSave: function (sTargetStatus) {
      var oSelectedModel = this.getModel("selected");
      var oStateModel = this.getModel("state");
      DetailToolbarValidationUseCase.markDirtyStatusAndNormalize(oSelectedModel, oStateModel, sTargetStatus);
      return this.onSaveDetail().then(function (oResult) {
        this.showI18nToast("statusChanged");
        return oResult;
      }.bind(this));
    },

    onChangeChecklistStatus: function (oEvent) {
      var sStatus = oEvent.getSource().data("targetStatus") || "";
      return DetailStatusCommandUseCase.runStatusChangeFlow({
        targetStatus: sStatus,
        validateChecklist: this.onValidateChecklist.bind(this),
        getSelectedRoot: function () {
          var oSelected = this.getModel("selected").getData() || {};
          return oSelected.root || {};
        }.bind(this),
        shouldApplyStatusChange: DetailStatusRowUseCase.shouldApplyStatusChange,
        requiresIntegrationConfirmation: DetailStatusRowUseCase.requiresIntegrationConfirmation,
        confirmIntegrationEdit: this._confirmIntegrationEdit.bind(this),
        applyStatusAndSave: this._applyStatusAndSave.bind(this)
      });
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


    _syncCreateCopyKeyMappingAfterSave: function (sSavedId, bCreateMode) {
      var sRealId = String(sSavedId || "").trim();
      if (!sRealId) {
        return;
      }

      var oComp = this.getOwnerComponent();
      var oSmartCache = oComp && oComp._oSmartCache;
      if (!oSmartCache || typeof oSmartCache.setKeyMapping !== "function") {
        return;
      }

      if (bCreateMode) {
        oSmartCache.setKeyMapping("__create", sRealId);
      }

      var oStateModel = this.getModel("state");
      var sCopySourceId = String((oStateModel && oStateModel.getProperty("/copySourceId")) || "").trim();
      if (sCopySourceId) {
        oSmartCache.setKeyMapping(sCopySourceId, sRealId);
      }

      var oCacheModel = this.getModel("cache");
      if (oCacheModel && typeof oCacheModel.setProperty === "function" && typeof oSmartCache.snapshot === "function") {
        oCacheModel.setProperty("/keyMapping", oSmartCache.snapshot().keyMapping);
      }
    },

    onSaveDetail: function () {
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var oEdited = this.getModel("selected").getData() || {};
      var sId = ((((oEdited || {}).root || {}).id) || "").trim();

      var bCreateMode = !sId;
      OperationalKpiInstrumentationUseCase.markSaveAttempt(oStateModel);
      var iSaveLatencyStartedAt = OperationalKpiInstrumentationUseCase.beginLatencySample();

      var oTelemetrySample = UxTelemetry.begin("detail.save", { mode: bCreateMode ? "create" : "update" });
      return DetailSaveFlowOrchestrationUseCase.runSaveFlow({
        runWithLoading: function (fnTask) {
          return this.runWithStateFlag(oStateModel, "/isLoading", fnTask);
        }.bind(this),
          saveChecklist: function () {
            return ChecklistCrudUseCase.saveChecklist(sId, oEdited, bCreateMode);
          },
          loadChecklistCollection: ChecklistCrudUseCase.getChecklistCollection,
          applySaveResult: function (oResult) {
            DetailSaveSuccessFlowUseCase.applySaveSuccess({
              result: oResult,
              dataModel: oDataModel,
              selectedModel: this.getModel("selected"),
              stateModel: oStateModel,
              dispatchFullSave: function () {
                window.dispatchEvent(new CustomEvent("pcct:fullSave"));
              },
              showSavedToast: function () {
                this.showI18nToast("objectSaved");
              }.bind(this),
              navigateToSaved: function (sSavedId) {
                if (!sSavedId) {
                  return;
                }
                var sLayout = this.getModel("state").getProperty("/preferredDetailLayout") || "TwoColumnsMidExpanded";
                this.navTo("detailLayout", { id: sSavedId, layout: sLayout }, true);
              }.bind(this)
            });
            var sSavedId = String(((((oResult || {}).savedChecklist || {}).root || {}).id) || "").trim();
            this._syncCreateCopyKeyMappingAfterSave(sSavedId, bCreateMode);
            OperationalKpiInstrumentationUseCase.markSaveSuccess(oStateModel);
            OperationalKpiInstrumentationUseCase.finishLatencySample(oStateModel, "save", iSaveLatencyStartedAt);
            UxTelemetry.end(oTelemetrySample, "success", oStateModel);
          }.bind(this),
          handleSaveError: function (oError) {
            return DetailSaveErrorPresentationUseCase.handleSaveError({
              host: this,
              error: oError,
              handleBackendError: FlowCoordinator.handleBackendError,
              reloadLabel: this.getResourceBundle().getText("reloadButton"),
              overwriteLabel: this.getResourceBundle().getText("overwriteButton"),
              onReload: function () {
                return ChecklistCrudUseCase.getChecklistRoot(sId).then((oRootOnly) => {
                  if (oRootOnly) {
                    this.getModel("selected").setData(ChecklistDraftHelper.clone(oRootOnly));
                    this._applyChecklistLazily(oRootOnly);
                  }
                });
              }.bind(this),
              onOverwrite: function () {
                return ChecklistCrudUseCase.forceUpdateChecklist(sId, oEdited);
              }
            }).then(function (oHandledResult) {
              DetailSaveErrorOutcomePresentationUseCase.runOutcomeLifecycle({
                result: oHandledResult,
                bundle: this.getResourceBundle(),
                showToast: MessageToast.show,
                markSaveFailed: function () {
                  OperationalKpiInstrumentationUseCase.markSaveFailed(oStateModel);
                },
                markConflict: function () {
                  OperationalKpiInstrumentationUseCase.markConflict(oStateModel);
                },
                finishLatency: function (sMetric, iStartedAt) {
                  OperationalKpiInstrumentationUseCase.finishLatencySample(oStateModel, sMetric, iStartedAt);
                },
                startedAt: iSaveLatencyStartedAt
              });
              UxTelemetry.end(oTelemetrySample, oHandledResult && oHandledResult.success === false ? "error" : "success", oStateModel);
              return oHandledResult;
            }.bind(this));
          }.bind(this)
        });
    },



    _openExpandedDialog: function (sType) {
      return DetailExpandedRowsFlowUseCase.openExpandedDialogByType({
        type: sType,
        host: this,
        view: this.getView(),
        controller: this
      });
    },

    _openExpandedDialogByType: function (sType) {
      return this._openExpandedDialog(sType);
    },

    onExpandChecks: function () {
      this._openExpandedDialogByType("checks");
    },

    onExpandBarriers: function () {
      this._openExpandedDialogByType("barriers");
    },

    onCloseChecksExpanded: function () {
      DetailExpandedRowsFlowUseCase.closeExpandedDialogByType({
        type: "checks",
        byId: this.byId.bind(this)
      });
    },

    onCloseBarriersExpanded: function () {
      DetailExpandedRowsFlowUseCase.closeExpandedDialogByType({
        type: "barriers",
        byId: this.byId.bind(this)
      });
    },

    _addRowByType: function (sType) {
      DetailExpandedRowsFlowUseCase.addRowByType({
        type: sType,
        selectedModel: this.getModel("selected"),
        onMutated: this._syncSelectionMeta.bind(this)
      });
    },

    onAddCheckRow: function () {
      this._addRowByType("checks");
    },

    onAddBarrierRow: function () {
      this._addRowByType("barriers");
    },

    _deleteRowByType: function (oEvent, sType) {
      DetailExpandedRowsFlowUseCase.deleteRowByType({
        event: oEvent,
        type: sType,
        deleteRowFromEvent: this.deleteRowFromEvent.bind(this),
        onSyncSelectionMeta: this._syncSelectionMeta.bind(this)
      });
    },

    onDeleteCheckRow: function (oEvent) {
      this._deleteRowByType(oEvent, "checks");
    },

    onDeleteBarrierRow: function (oEvent) {
      this._deleteRowByType(oEvent, "barriers");
    },

    onExit: function () {
      DetailDialogLifecycleUseCase.destroyDialogsByIds({
        byId: this.byId.bind(this),
        ids: ["checksExpandedDialog", "barriersExpandedDialog"],
        host: this,
        props: ["_pChecksDialog", "_pBarriersDialog"]
      });
    },

    resultText: function (bResult) {
      return bResult ? "Passed" : "Failed";
    },

    resultState: function (bResult) {
      return bResult ? "Success" : "Error";
    }
  });
});
