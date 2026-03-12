sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailEditRestoreRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailMatchedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailSelectedFieldRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DetailRuntimePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DialogContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts"
], function (DialogOrchestrator, DetailViewBehavior, DetailAccessViewState, DetailActionConstants, DetailCommandPolicy, DetailInfoCardLayoutRuntime, DetailEditRestoreRuntime, DetailMatchedRuntime, DetailSelectedFieldRuntime, ModelPathContracts, ViewPathContracts, StatePaths, FeedbackCoordinator, ControllerViewStateRuntime, ModelStateRuntime, DetailRuntimePolicy, NavigationIntentService, CreateSentinel, DialogContracts, NavigationContracts, WorkflowContracts, ModelContracts, OperationSourceContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var SELECTED_MODEL = MODELS.SELECTED;
    var APP_SOURCES = OperationSourceContracts.APP;
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    function isDirtyTrackMode(oController) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        return WorkflowContracts.isEditableMode(sMode);
    }

    return {
        ensureEffectDialog: function (sId) {
            var sFragment = DialogContracts.getFragmentName(sId);
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
            return DetailViewBehavior.buildInfoCard(this, sId, oContext);
        },

        _focusInfoCardByKey: function (sKey) {
            DetailInfoCardLayoutRuntime.focusCardByKey(this, sKey);
        },

        _isEditMode: function () {
            return WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(this, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ)) === WorkflowContracts.EDIT_MODES.EDIT;
        },

        _showToast: function (sTextKey) {
            return FeedbackCoordinator.showToast(this, sTextKey, [], "info");
        },

        _applyLayoutState: function (sLayout, mOptions) {
            DetailViewBehavior.applyLayoutState(this, sLayout, mOptions);
        },

        _onDetailMatched: function (oEvent) {
            var mContext = DetailMatchedRuntime.prepareMatchedContext(this, oEvent);
            var sApplyMode = DetailMatchedRuntime.applyMatchedState(this, mContext, {
                applyLayoutState: this._applyLayoutState.bind(this),
                createAccessState: DetailAccessViewState.createDefaultState,
                openChecklist: function (mInput) {
                    return DetailCommandPolicy.open(this, mInput);
                }.bind(this),
                scheduleAttachmentDropZoneBind: this._scheduleAttachmentDropZoneBind.bind(this),
                validationSummaryPath: STATE_PATHS.VALIDATION_SUMMARY,
                writeInfoCards: function (aCards) {
                    DetailInfoCardLayoutRuntime.writeCards(this, aCards);
                }.bind(this)
            });

            if (sApplyMode === "layoutOnly") {
                return;
            }

            if (mContext.bCreate) {
                DetailMatchedRuntime.openCreateDraft(this, mContext, {
                    openChecklist: function (mInput) {
                        return DetailCommandPolicy.open(this, mInput);
                    }.bind(this)
                });
                return;
            }

            if (mContext.sPostOpenHydratedRootId && mContext.sPostOpenHydratedRootId === mContext.sId && mContext.sSelectedRootId === mContext.sId) {
                ModelStateRuntime.write(this, STATE_MODEL, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "");
                ControllerViewStateRuntime.set(this, ViewPathContracts.DETAIL_SKELETON_BUSY, false);
                DetailEditRestoreRuntime.restoreAnalyticsEditIfNeeded(this, mContext.sId, {
                    enterEdit: function (mInput) {
                        return DetailCommandPolicy.enterEdit(this, mInput);
                    }.bind(this),
                    onToggleEdit: this.onToggleEdit && this.onToggleEdit.bind(this)
                });
                return;
            }

            ModelStateRuntime.write(this, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, mContext.sId);
            DetailCommandPolicy.open(this, { id: mContext.sId, rootId: mContext.sId }).then(function (oResult) {
                var oAccessState;
                if (oResult && oResult.ok === false) {
                    if (!oResult.error || oResult.error.code !== "NO_VIEW_PERMISSION") {
                        DetailEditRestoreRuntime.clearAnalyticsReturnRestore(this);
                        return oResult;
                    }
                    oAccessState = ControllerViewStateRuntime.get(this, "/accessState", {}) || {};
                    ModelStateRuntime.write(this, STATE_MODEL, "/detailAccessGuard", {
                        rootId: String(oAccessState.rootId || mContext.sId || "").trim(),
                        userId: String(oAccessState.userId || "").trim(),
                        canView: false,
                        canEdit: !!oAccessState.canEdit,
                        canDelete: !!oAccessState.canDelete,
                        reasonCode: String(oAccessState.reasonCode || "NO_VIEW_PERMISSION").trim(),
                        message: String(oAccessState.message || "").trim(),
                        checkedAt: new Date().toISOString()
                    });
                    DetailEditRestoreRuntime.clearAnalyticsReturnRestore(this);
                    return oResult;
                }
                return DetailEditRestoreRuntime.restoreAnalyticsEditIfNeeded(this, mContext.sId, {
                    enterEdit: function (mInput) {
                        return DetailCommandPolicy.enterEdit(this, mInput);
                    }.bind(this),
                    onToggleEdit: this.onToggleEdit && this.onToggleEdit.bind(this)
                }).then(function () {
                    return oResult;
                });
            }.bind(this));
        },

        _currentRootId: function () {
            return ModelStateRuntime.read(this, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, "")
                || ModelStateRuntime.read(this, STATE_MODEL, ModelPathContracts.SELECTED_ID, "")
                || "";
        },

        _applySelectedFieldChange: function (oEvent, mOptions) {
            return DetailSelectedFieldRuntime.applySelectedFieldChange(this, oEvent, mOptions, {
                isDirtyTrackMode: function () {
                    return isDirtyTrackMode(this);
                }.bind(this)
            });
        },

        _resolveRowInput: function (oEvent) {
            return DetailSelectedFieldRuntime.resolveRowInput(oEvent);
        }
    };
});
