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
  "sap_ui5/service/usecase/DetailCommandFlowUseCase",
  "sap_ui5/service/usecase/DetailStatusRowUseCase",
  "sap_ui5/service/usecase/DetailStatusCommandUseCase",
  "sap_ui5/service/usecase/DetailRowDialogCommandUseCase",
  "sap_ui5/service/usecase/DetailSaveOrchestrationUseCase",
  "sap_ui5/service/usecase/DetailDialogLifecycleUseCase",
  "sap_ui5/service/usecase/DetailLockEditFlowUseCase",
  "sap_ui5/service/usecase/DetailLockReleaseUseCase",
  "sap_ui5/service/usecase/DetailSaveSuccessFlowUseCase",
  "sap_ui5/service/usecase/DetailCloseFlowUseCase",
  "sap_ui5/service/usecase/DetailCloseNavigationUseCase",
  "sap_ui5/service/usecase/DetailToolbarValidationUseCase",
  "sap_ui5/service/usecase/DetailSaveErrorPresentationUseCase"
], function (BaseController, BackendAdapter, MessageToast, MessageBox, JSONModel, RowListHelper, ChecklistDraftHelper, FlowCoordinator, ChecklistValidationService, ChecklistUiState, DetailCardSchema, DetailFormatters, ChecklistCrudUseCase, DetailLifecycleUseCase, DetailCommandFlowUseCase, DetailStatusRowUseCase, DetailStatusCommandUseCase, DetailRowDialogCommandUseCase, DetailSaveOrchestrationUseCase, DetailDialogLifecycleUseCase, DetailLockEditFlowUseCase, DetailLockReleaseUseCase, DetailSaveSuccessFlowUseCase, DetailCloseFlowUseCase, DetailCloseNavigationUseCase, DetailToolbarValidationUseCase, DetailSaveErrorPresentationUseCase) {
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
        locationVhRows: []
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

    formatLifecycleStatusState: function (sStatus) {
      return DetailFormatters.lifecycleStatusState(sStatus);
    },

    formatPassedTotal: function (aRows) {
      return DetailFormatters.passedTotal(aRows);
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
        DetailLifecycleUseCase.resetDirty(oStateModel);
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




    onOpenLocationValueHelp: function () {
      var oDialog = this.byId("locationValueHelpDialog");
      if (oDialog) {
        var aRows = this.getModel("mpl").getProperty("/locations") || [];
        this.getView().getModel("view").setProperty("/locationVhRows", aRows);
        oDialog.open();
      }
    },

    onCloseLocationValueHelp: function () {
      var oDialog = this.byId("locationValueHelpDialog");
      if (oDialog) {
        oDialog.close();
      }
    },

    onLocationValueHelpSearch: function (oEvent) {
      var sValue = this._normalizeText(oEvent.getParameter("newValue"));
      var aRows = this.getModel("mpl").getProperty("/locations") || [];

      if (!sValue) {
        this.getView().getModel("view").setProperty("/locationVhRows", aRows);
        return;
      }

      var aFiltered = aRows.filter(function (oRow) {
        var sNodeId = this._normalizeText(oRow && oRow.node_id);
        var sParentId = this._normalizeText(oRow && oRow.parent_id);
        var sName = this._normalizeText(oRow && oRow.location_name);
        return sNodeId.indexOf(sValue) >= 0 || sParentId.indexOf(sValue) >= 0 || sName.indexOf(sValue) >= 0;
      }.bind(this));

      this.getView().getModel("view").setProperty("/locationVhRows", aFiltered);
    },

    onLocationValueHelpSelectionChange: function (oEvent) {
      var oItem = oEvent.getParameter("listItem");
      if (!oItem) {
        return;
      }

      var oCtx = oItem.getBindingContext("mpl") || oItem.getBindingContext("view");
      var oNode = oCtx ? oCtx.getObject() : null;
      if (!oNode) {
        return;
      }
      var oSelectedModel = this.getModel("selected");
      oSelectedModel.setProperty("/basic/LOCATION_KEY", oNode.node_id || "");
      oSelectedModel.setProperty("/basic/LOCATION_NAME", oNode.location_name || "");
      oSelectedModel.setProperty("/basic/LOCATION_TEXT", oNode.location_name || "");
      this.onCloseLocationValueHelp();
    },

    onLocationComboChange: function (oEvent) {
      var oItem = oEvent.getParameter("selectedItem");
      if (!oItem) {
        return;
      }
      var oCtx = oItem.getBindingContext("mpl");
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
      var fnProceed = DetailCloseNavigationUseCase.buildCloseProceedAction({
        stateModel: oState,
        releaseLock: this.releaseLock.bind(this),
        prepareCloseNavigation: DetailLifecycleUseCase.prepareCloseNavigation,
        navigateToSearch: function () {
          this.navTo("search", {}, true);
        }.bind(this)
      });

      return DetailCloseFlowUseCase.runCloseFlow({
        isDirty: oState.getProperty("/isDirty"),
        shouldPromptBeforeClose: DetailCommandFlowUseCase.shouldPromptBeforeClose,
        shouldProceedAfterUnsavedDecision: DetailCommandFlowUseCase.shouldProceedAfterUnsavedDecision,
        confirmUnsaved: function () {
          return FlowCoordinator.confirmUnsavedAndHandle(this, this.onSaveDetail.bind(this));
        }.bind(this),
        proceed: fnProceed
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
      var sPendingText = this.getResourceBundle().getText("lockPending");

      var fnDisableEditAndRelease = () => this._releaseEditLock(sObjectId, sSessionId, oStateModel);

      return DetailLockEditFlowUseCase.runToggleEditFlow({
        editMode: bEditMode,
        isDirty: oStateModel.getProperty("/isDirty"),
        confirmUnsaved: function () {
          return FlowCoordinator.confirmUnsavedAndHandle(this, this.onSaveDetail.bind(this));
        }.bind(this),
        runPendingRelease: function () {
          this.setLockUiState(oStateModel, "PENDING", sPendingText);
          return this.runWithStateFlag(oStateModel, "/lockOperationPending", fnDisableEditAndRelease);
        }.bind(this),
        runPendingToggle: function (fnFlow) {
          this.setLockUiState(oStateModel, "PENDING", sPendingText);
          return this.runWithStateFlag(oStateModel, "/lockOperationPending", fnFlow.bind(this));
        }.bind(this),
        releaseEdit: fnDisableEditAndRelease,
        ensureFreshBeforeEdit: function () {
          return this._ensureChecklistFreshBeforeEdit(sObjectId);
        }.bind(this),
        confirmIntegrationEdit: this._confirmIntegrationEdit.bind(this),
        onStayReadOnly: function () {
          this.setLockUiState(oStateModel, "IDLE", this.getResourceBundle().getText("lockStayReadOnly"));
        }.bind(this),
        acquireLock: function () {
          return BackendAdapter.lockAcquire(sObjectId, sSessionId);
        },
        onLockAcquired: function () {
          DetailLifecycleUseCase.setEditLocked(oStateModel);
          this.setLockUiState(oStateModel, "SUCCESS", this.getResourceBundle().getText("lockOwnedByMe"));
        }.bind(this),
        tryRecoverFromAcquireError: function (oError) {
          return this._tryStealOwnLock(sObjectId, sSessionId, oError).then(function (oResult) {
            return !!(oResult && oResult.success);
          }).catch(function () {
            return false;
          });
        }.bind(this),
        onAcquireFailed: function (oError) {
          DetailLifecycleUseCase.setReadUnlocked(oStateModel);
          this.setLockUiState(oStateModel, "ERROR", this.getResourceBundle().getText("lockOwnedByOther"));
          return FlowCoordinator.handleBackendError(this, oError);
        }.bind(this)
      });
    },

    onCancelEditFromDetail: function () {
      var oSelected = this.getModel("data").getProperty("/selectedChecklist") || {};
      var oStateModel = this.getModel("state");
      var sObjectId = oStateModel.getProperty("/activeObjectId");
      var sSessionId = oStateModel.getProperty("/sessionId");

      this.getModel("selected").setData(ChecklistDraftHelper.clone(oSelected));
      DetailLifecycleUseCase.resetDirty(oStateModel);
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

    onSaveDetail: function () {
      var oStateModel = this.getModel("state");
      var oDataModel = this.getModel("data");
      var oEdited = this.getModel("selected").getData() || {};
      var sId = ((((oEdited || {}).root || {}).id) || "").trim();

      var bCreateMode = !sId;

      return this.runWithStateFlag(oStateModel, "/isLoading", function () {
        return DetailSaveOrchestrationUseCase.runSaveFlow({
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
              }.bind(this)
            });
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
            });
          }.bind(this)
        });
      }.bind(this));
    },



    _openExpandedDialog: function (sType) {
      var oMeta = DetailStatusRowUseCase.resolveExpandedDialogMeta(sType);
      return DetailDialogLifecycleUseCase.openDialog({
        host: this,
        prop: oMeta.prop,
        fragment: oMeta.fragment,
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
      this._closeExpandedDialogById(DetailRowDialogCommandUseCase.resolveExpandedDialogId("checks"));
    },

    onCloseBarriersExpanded: function () {
      this._closeExpandedDialogById(DetailRowDialogCommandUseCase.resolveExpandedDialogId("barriers"));
    },

    _addRowByType: function (sType) {
      this._mutateRows(DetailRowDialogCommandUseCase.resolveRowPath(sType), RowListHelper.addRow);
    },

    onAddCheckRow: function () {
      this._addRowByType("checks");
    },

    onAddBarrierRow: function () {
      this._addRowByType("barriers");
    },

    _deleteRowByType: function (oEvent, sType) {
      var oResult = this.deleteRowFromEvent(oEvent, "selected", DetailRowDialogCommandUseCase.resolveRowPath(sType));
      if (!DetailRowDialogCommandUseCase.shouldProcessRowDeleteResult(oResult)) {
        return;
      }
      DetailStatusCommandUseCase.handleDeleteRowResult(oResult, DetailStatusRowUseCase.shouldSyncAfterDeleteResult, this._syncSelectionMeta.bind(this));
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
