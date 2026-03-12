sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/util/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailViewRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DomainStatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DetailRuntimePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (DialogOrchestrator, DraftChecklistFactory, DetailViewRuntime, DetailAccessViewState, DetailActionConstants, DetailCommandPolicy, DetailInfoCardLayoutRuntime, DomainStatePaths, ViewPathContracts, StatePaths, FeedbackCoordinator, ControllerViewStateRuntime, ModelStateRuntime, DetailRuntimePolicy, NavigationIntentService, CreateSentinel) {
    "use strict";

    var EFFECT_DIALOG_FRAGMENTS = {
        locationValueHelp: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.LocationValueHelpDialog",
        checksExpanded: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.ChecksExpandedDialog",
        barriersExpanded: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.BarriersExpandedDialog"
    };
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    function isDirtyTrackMode(oController) {
        var sMode = String(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ") || "READ").trim().toUpperCase();
        return sMode === "EDIT" || sMode === "CREATE";
    }

    function readAnalyticsReturnRestore(oController) {
        return ModelStateRuntime.read(oController, "state", DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, null) || null;
    }

    function clearAnalyticsReturnRestore(oController) {
        ModelStateRuntime.write(oController, "state", DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, null);
    }

    function isEditLockedState(oController) {
        var sMode = String(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ") || "READ").trim().toUpperCase();
        var sLockState = String(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "READ_ONLY") || "READ_ONLY").trim().toUpperCase();
        return sMode === "EDIT" && sLockState === "EDIT_LOCKED";
    }

    function buildEditToggleEvent() {
        return {
            getParameter: function (sName) {
                return sName === "state" ? true : undefined;
            }
        };
    }

    function requestAnalyticsEditRestore(oController, sRootId) {
        if (!oController) {
            return Promise.resolve(false);
        }
        if (typeof oController.onToggleEdit === "function") {
            return Promise.resolve(oController.onToggleEdit(buildEditToggleEvent())).then(function () {
                return isEditLockedState(oController);
            });
        }
        return Promise.resolve(DetailCommandPolicy.enterEdit(oController, { rootId: sRootId })).then(function () {
            return isEditLockedState(oController);
        });
    }

    function restoreAnalyticsEditIfNeeded(oController, sRootId) {
        var oRestore = readAnalyticsReturnRestore(oController);
        var sRestoreRootId = String((oRestore && oRestore.rootId) || "").trim();
        var iAttempts;
        var oRestorePlan;

        if (!sRootId || !sRestoreRootId || sRestoreRootId !== sRootId) {
            return Promise.resolve(false);
        }

        if (isEditLockedState(oController)) {
            clearAnalyticsReturnRestore(oController);
            return Promise.resolve(true);
        }

        iAttempts = Number((oRestore && oRestore.attempts) || 0);
        oRestorePlan = DetailRuntimePolicy.analyticsEditRestorePlan({
            controller: oController,
            rootId: sRootId,
            restoreState: oRestore
        });
        if (iAttempts >= oRestorePlan.maxAttempts) {
            clearAnalyticsReturnRestore(oController);
            return Promise.resolve(false);
        }

        ModelStateRuntime.write(oController, "state", DomainStatePaths.ANALYTICS_RETURN_RESTORE_EDIT, {
            rootId: sRestoreRootId,
            requestedAt: oRestore && oRestore.requestedAt ? oRestore.requestedAt : new Date().toISOString(),
            attempts: iAttempts + 1
        });

        return requestAnalyticsEditRestore(oController, sRootId)
            .then(function (bRestored) {
                if (bRestored) {
                    clearAnalyticsReturnRestore(oController);
                    return true;
                }
                return new Promise(function (resolve) {
                    setTimeout(function () {
                        requestAnalyticsEditRestore(oController, sRootId)
                            .then(function (bRetryRestored) {
                                if (bRetryRestored) {
                                    clearAnalyticsReturnRestore(oController);
                                    resolve(true);
                                    return;
                                }
                                resolve(false);
                            })
                            .catch(function () {
                                resolve(false);
                            });
                    }, oRestorePlan.retryDelayMs);
                });
            })
            .catch(function () {
                return false;
            });
    }

    function resolveSelectedBindingPath(oSource, sProperty) {
        var oBinding = oSource && oSource.getBinding && oSource.getBinding(sProperty);
        var oContext = oSource && oSource.getBindingContext && (oSource.getBindingContext("selected") || oSource.getBindingContext());
        var sContextPath = String((oContext && oContext.getPath && oContext.getPath()) || "").trim();
        var sBindingPath = String((oBinding && oBinding.getPath && oBinding.getPath()) || "").trim();
        var sModelName = String((oBinding && oBinding.getModel && oBinding.getModel() && oBinding.getModel().sName) || "").trim();
        if (!sBindingPath || (sModelName && sModelName !== "selected")) {
            return "";
        }
        if (sBindingPath.charAt(0) === "/") {
            return sBindingPath;
        }
        return (sContextPath ? sContextPath + "/" : "/") + sBindingPath;
    }

    function normalizeEventValue(oEvent, sParameterName, sPropertyName, oSource) {
        var vValue = oEvent && oEvent.getParameter && oEvent.getParameter(sParameterName);
        if (typeof vValue !== "undefined") {
            return vValue;
        }
        if (oSource && typeof oSource.getProperty === "function") {
            return oSource.getProperty(sPropertyName);
        }
        return undefined;
    }

    return {
        ensureEffectDialog: function (sId) {
            var sFragment = EFFECT_DIALOG_FRAGMENTS[sId];
            if (!sFragment) {
                return Promise.resolve(null);
            }
            return DialogOrchestrator.ensure(this, sId, {
                fragmentName: sFragment,
                afterClose: function (_oDialog, oCtrl, sDialogKey) {
                    if (oCtrl && typeof oCtrl._restoreDialogFocus === "function") {
                        oCtrl._restoreDialogFocus(sDialogKey);
                    }
                },
                afterOpen: function (_oDialog, oCtrl, sDialogKey) {
                    if (oCtrl && typeof oCtrl._onDialogAfterOpen === "function") {
                        oCtrl._onDialogAfterOpen(sDialogKey);
                    }
                }
            });
        },

        infoCardFactory: function (sId, oContext) {
            return DetailViewRuntime.buildInfoCard(this, sId, oContext);
        },

        _focusInfoCardByKey: function (sKey) {
            DetailInfoCardLayoutRuntime.focusCardByKey(this, sKey);
        },

        _isEditMode: function () {
            return ModelStateRuntime.read(this, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE) === "EDIT";
        },

        _showToast: function (sTextKey) {
            return FeedbackCoordinator.showToast(this, sTextKey, [], "info");
        },

        _applyLayoutState: function (sLayout, mOptions) {
            DetailViewRuntime.applyLayoutState(this, sLayout, mOptions);
        },

        _onDetailMatched: function (oEvent) {
            var mArgs = oEvent.getParameter("arguments") || {};
            var sRouteName = String((oEvent.getParameter("name") || (mArgs.layout ? "detailLayout" : "detail")) || "detail").trim() || "detail";
            var sId = mArgs.id;
            var sLayoutArg = String(mArgs.layout || "").toLowerCase();
            var bCreate = CreateSentinel.isCreateId(sId);
            var sRouteLayout = sLayoutArg === "midcolumnfullscreen" ? "MidColumnFullScreen" : "TwoColumnsMidExpanded";
            var sCurrentRouteName = String(ModelStateRuntime.read(this, "state", DomainStatePaths.CURRENT_ROUTE_NAME, "search") || "search").trim() || "search";
            var sCurrentRootId = String(ModelStateRuntime.read(this, "state", DomainStatePaths.ACTIVE_OBJECT_ID, "") || "").trim();
            var mStatePatch = {
                [DomainStatePaths.ACTIVE_OBJECT_ID]: bCreate ? CreateSentinel.VALUE : sId,
                [DomainStatePaths.SELECTED_ID]: bCreate ? CreateSentinel.VALUE : sId,
                [DomainStatePaths.CURRENT_ROUTE_NAME]: sRouteName
            };
            var mViewPatch = {
                [ViewPathContracts.DETAIL_SKELETON_BUSY]: !bCreate,
                [ViewPathContracts.VALIDATION_SHOWN]: false,
                [ViewPathContracts.VALIDATION_MISSING]: {},
                "/deleteChecklistConfirmArmed": false,
                "/attachmentsExpanded": false,
                [ViewPathContracts.ATTACHMENTS_LOADED]: false,
                [ViewPathContracts.SESSION_ATTACHMENTS]: [],
                "/attachmentBusy": false,
                "/observerSuggestions": [],
                "/observedSuggestions": [],
                "/personSuggestHint": "",
                [ViewPathContracts.ACCESS_STATE]: DetailAccessViewState.createDefaultState(sId)
            };
            var sPostOpenHydratedRootId = String(ModelStateRuntime.read(this, "state", DomainStatePaths.POST_OPEN_HYDRATED_ROOT_ID, "") || "").trim();
            var oSelected = this.getModel("selected");
            var oSelectedData = (oSelected && oSelected.getData && oSelected.getData()) || {};
            var sSelectedRootId = String((oSelectedData && oSelectedData.root && oSelectedData.root.id) || "").trim();
            var bLayoutOnlyTransition = !bCreate &&
                sCurrentRootId === sId &&
                sSelectedRootId === sId &&
                ["detail", "detailLayout"].indexOf(sCurrentRouteName) >= 0 &&
                ["detail", "detailLayout"].indexOf(sRouteName) >= 0;

            if (bLayoutOnlyTransition) {
                ModelStateRuntime.setMany(this, "state", mStatePatch);
                this._applyLayoutState(sRouteLayout, { syncRoute: false });
                return;
            }

            if (bCreate) {
                mStatePatch[StatePaths.WORKFLOW_DETAIL_EDIT_MODE] = "CREATE";
                mStatePatch[StatePaths.WORKFLOW_DETAIL_LOCK_STATE] = "IDLE";
                mStatePatch[DomainStatePaths.AUTOSAVE_ENABLED] = false;
                mStatePatch[DomainStatePaths.IS_DIRTY] = false;
            }
            ModelStateRuntime.setMany(this, "state", mStatePatch);
            this._applyLayoutState(sRouteLayout, { syncRoute: false });
            ControllerViewStateRuntime.setMany(this, mViewPatch);
            DetailInfoCardLayoutRuntime.writeCards(this, ControllerViewStateRuntime.get(this, "/infoCards", []));
            ModelStateRuntime.write(this, "state", STATE_PATHS.VALIDATION_SUMMARY, {
                hasErrors: false,
                missingPaths: [],
                missingKeys: [],
                source: "detailMatched",
                firstMissingPath: "",
                firstMissingKey: ""
            });
            this._scheduleAttachmentDropZoneBind();

            if (bCreate) {
                if (oSelected && oSelected.setData) {
                    oSelected.setData(DraftChecklistFactory.createEmptyDraft());
                    ModelStateRuntime.writeOnModel(oSelected, "/attachments", []);
                }
                DetailCommandPolicy.open(this, { id: CreateSentinel.VALUE, rootId: CreateSentinel.VALUE });
                return;
            }
            if (sPostOpenHydratedRootId && sPostOpenHydratedRootId === sId && sSelectedRootId === sId) {
                ModelStateRuntime.write(this, "state", DomainStatePaths.POST_OPEN_HYDRATED_ROOT_ID, "");
                ControllerViewStateRuntime.set(this, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
                restoreAnalyticsEditIfNeeded(this, sId);
                return;
            }
            ModelStateRuntime.write(this, "state", DomainStatePaths.ACTIVE_OBJECT_ID, sId);
            DetailCommandPolicy.open(this, { id: sId, rootId: sId }).then(function (oResult) {
                var oAccessState;
                if (oResult && oResult.ok === false) {
                    if (!oResult.error || oResult.error.code !== "NO_VIEW_PERMISSION") {
                        clearAnalyticsReturnRestore(this);
                        return oResult;
                    }
                    oAccessState = ControllerViewStateRuntime.get(this, "/accessState", {}) || {};
                    ModelStateRuntime.write(this, "state", "/detailAccessGuard", {
                        rootId: String(oAccessState.rootId || sId || "").trim(),
                        userId: String(oAccessState.userId || "").trim(),
                        canView: false,
                        canEdit: !!oAccessState.canEdit,
                        canDelete: !!oAccessState.canDelete,
                        reasonCode: String(oAccessState.reasonCode || "NO_VIEW_PERMISSION").trim(),
                        message: String(oAccessState.message || "").trim(),
                        checkedAt: new Date().toISOString()
                    });
                    clearAnalyticsReturnRestore(this);
                    return oResult;
                }
                return restoreAnalyticsEditIfNeeded(this, sId).then(function () {
                    return oResult;
                });
            }.bind(this));
        },

        _currentRootId: function () {
            return ModelStateRuntime.read(this, "state", DomainStatePaths.ACTIVE_OBJECT_ID, "")
                || ModelStateRuntime.read(this, "state", DomainStatePaths.SELECTED_ID, "")
                || "";
        },

        _applySelectedFieldChange: function (oEvent, mOptions) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sProperty = String((mOptions && mOptions.property) || "value").trim() || "value";
            var sParameter = String((mOptions && mOptions.parameter) || sProperty).trim() || sProperty;
            var sPath = resolveSelectedBindingPath(oSource, sProperty);
            var vValue;
            if (!oSource || !sPath || !isDirtyTrackMode(this)) {
                return false;
            }
            vValue = normalizeEventValue(oEvent, sParameter, sProperty, oSource);
            if (typeof vValue === "undefined") {
                return false;
            }
            ModelStateRuntime.write(this, "selected", sPath, vValue);
            ModelStateRuntime.write(this, "state", DomainStatePaths.IS_DIRTY, true);
            return true;
        },

        _resolveRowInput: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oCursor = oSource;
            var oCtx;
            var oRow;
            var sPath = "";
            while (oCursor) {
                if (oCursor.getBindingContext) {
                    oCtx = oCursor.getBindingContext("selected") || oCursor.getBindingContext();
                    if (oCtx && oCtx.getPath) {
                        sPath = String(oCtx.getPath() || "");
                        oRow = oCtx.getObject && oCtx.getObject();
                        if (sPath) {
                            break;
                        }
                    }
                }
                oCursor = oCursor.getParent && oCursor.getParent();
            }
            return {
                rowPath: sPath,
                rowId: String((oRow && (oRow.client_row_id || oRow.Key || oRow.id)) || "").trim()
            };
        }
    };
});
