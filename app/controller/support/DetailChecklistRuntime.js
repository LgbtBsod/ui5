sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/util/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailViewRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (DialogOrchestrator, DraftChecklistFactory, DetailViewRuntime, DetailAccessViewState, DetailActionConstants, DetailCommandPolicy, DetailInfoCardLayoutRuntime, StatePaths, FeedbackCoordinator, ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService, CreateSentinel) {
    "use strict";

    var EFFECT_DIALOG_FRAGMENTS = {
        locationValueHelp: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.LocationValueHelpDialog",
        checksExpanded: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.ChecksExpandedDialog",
        barriersExpanded: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.BarriersExpandedDialog"
    };
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    function isDirtyTrackMode(oController) {
        var sMode = String(ModelStateRuntime.read(oController, "state", "/mode", "READ") || "READ").trim().toUpperCase();
        return sMode === "EDIT" || sMode === "CREATE";
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
            return ModelStateRuntime.read(this, "state", "/mode") === "EDIT";
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
            var sCurrentRouteName = String(ModelStateRuntime.read(this, "state", "/currentRouteName", "search") || "search").trim() || "search";
            var sCurrentRootId = String(ModelStateRuntime.read(this, "state", "/activeObjectId", "") || "").trim();
            var mStatePatch = {
                "/activeObjectId": bCreate ? CreateSentinel.VALUE : sId,
                "/selectedId": bCreate ? CreateSentinel.VALUE : sId,
                "/currentRouteName": sRouteName
            };
            var mViewPatch = {
                "/detailSkeletonBusy": !bCreate,
                "/validationShown": false,
                "/validationMissing": {},
                "/deleteChecklistConfirmArmed": false,
                "/attachmentsExpanded": false,
                "/attachmentsLoaded": false,
                "/sessionAttachments": [],
                "/attachmentBusy": false,
                "/observerSuggestions": [],
                "/observedSuggestions": [],
                "/personSuggestHint": "",
                "/accessState": DetailAccessViewState.createDefaultState(sId)
            };
            var sPostOpenHydratedRootId = String(ModelStateRuntime.read(this, "state", "/postOpenHydratedRootId", "") || "").trim();
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
                mStatePatch["/mode"] = "CREATE";
                mStatePatch[StatePaths.WORKFLOW_DETAIL_EDIT_MODE] = "CREATE";
                mStatePatch["/lockOperationState"] = "IDLE";
                mStatePatch["/autosaveEnabled"] = false;
                mStatePatch["/isDirty"] = false;
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
                ModelStateRuntime.write(this, "state", "/postOpenHydratedRootId", "");
                ControllerViewStateRuntime.set(this, "/detailSkeletonBusy", false);
                return;
            }
            ModelStateRuntime.write(this, "state", "/activeObjectId", sId);
            DetailCommandPolicy.open(this, { id: sId, rootId: sId }).then(function (oResult) {
                var oAccessState;
                if (!oResult || oResult.ok !== false || !oResult.error || oResult.error.code !== "NO_VIEW_PERMISSION") {
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
                return oResult;
            }.bind(this));
        },

        _currentRootId: function () {
            return ModelStateRuntime.read(this, "state", "/activeObjectId", "")
                || ModelStateRuntime.read(this, "state", "/selectedId", "")
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
            ModelStateRuntime.write(this, "state", "/isDirty", true);
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
