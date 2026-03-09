sap.ui.define([
    "checklist/app/service/framework/DialogOrchestrator",
    "checklist/app/util/DraftChecklistFactory",
    "checklist/app/controller/support/DetailViewSupport",
    "checklist/app/controller/support/DetailAccessViewState",
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/controller/support/DetailInfoCardLayoutSupport",
    "checklist/app/service/framework/FeedbackCoordinator",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/util/CreateSentinel"
], function (DialogOrchestrator, DraftChecklistFactory, DetailViewSupport, DetailAccessViewState, DetailActionConstants, DetailCommandPolicy, DetailInfoCardLayoutSupport, FeedbackCoordinator, ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService, CreateSentinel) {
    "use strict";

    var EFFECT_DIALOG_FRAGMENTS = {
        locationValueHelp: "checklist.app.view.fragment.LocationValueHelpDialog",
        checksExpanded: "checklist.app.view.fragment.ChecksExpandedDialog",
        barriersExpanded: "checklist.app.view.fragment.BarriersExpandedDialog"
    };
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

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
            return DetailViewSupport.buildInfoCard(this, sId, oContext);
        },

        _focusInfoCardByKey: function (sKey) {
            DetailInfoCardLayoutSupport.focusCardByKey(this, sKey);
        },

        _isEditMode: function () {
            return ModelStateRuntime.read(this, "state", "/mode") === "EDIT";
        },

        _showToast: function (sTextKey) {
            return FeedbackCoordinator.showToast(this, sTextKey, [], "info");
        },

        _applyLayoutState: function (sLayout, mOptions) {
            DetailViewSupport.applyLayoutState(this, sLayout, mOptions);
        },

        _onDetailMatched: function (oEvent) {
            var mArgs = oEvent.getParameter("arguments") || {};
            var sRouteName = String((oEvent.getParameter("name") || (mArgs.layout ? "detailLayout" : "detail")) || "detail").trim() || "detail";
            var sId = mArgs.id;
            var sLayoutArg = String(mArgs.layout || "").toLowerCase();
            var bCreate = CreateSentinel.isCreateId(sId);
            var sRouteLayout = sLayoutArg === "midcolumnfullscreen" ? "MidColumnFullScreen" : "TwoColumnsMidExpanded";
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

            if (bCreate) {
                mStatePatch["/mode"] = "CREATE";
                mStatePatch["/lockOperationState"] = "IDLE";
                mStatePatch["/autosaveEnabled"] = false;
                mStatePatch["/isDirty"] = false;
            }
            ModelStateRuntime.setMany(this, "state", mStatePatch);
            this._applyLayoutState(sRouteLayout, { syncRoute: false });
            ControllerViewStateRuntime.setMany(this, mViewPatch);
            DetailInfoCardLayoutSupport.writeCards(this, ControllerViewStateRuntime.get(this, "/infoCards", []));
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
                NavigationIntentService.navigateToAccessDenied(this, sId);
                return oResult;
            }.bind(this));
        },

        _currentRootId: function () {
            return ModelStateRuntime.read(this, "state", "/activeObjectId", "")
                || ModelStateRuntime.read(this, "state", "/selectedId", "")
                || "";
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
