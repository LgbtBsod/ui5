sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailChecklistStateBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailChecklistRowBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailEditRestoreRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailSelectedFieldRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DialogConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts"
], function (DialogOrchestrator, DetailViewBehavior, DetailActionConstants, DetailCommandPolicy, DetailChecklistStateBehavior, DetailChecklistRowBehavior, DetailInfoCardLayoutRuntime, DetailEditRestoreRuntime, DetailSelectedFieldRuntime, ModelPathContracts, StatePaths, FeedbackCoordinator, ModelStateRuntime, DetailRuntimePolicy, NavigationIntentService, CreateSentinel, DialogContracts, WorkflowContracts, ModelContracts, OperationSourceContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var APP_SOURCES = OperationSourceContracts.APP;

    function isDirtyTrackMode(oController) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        return WorkflowContracts.isEditableMode(sMode);
    }

    return Object.assign({}, DetailChecklistStateBehavior, DetailChecklistRowBehavior, {
        ensureEffectDialog: function (sId) {
            var sFragment = DialogContracts.getFragmentName(sId);
            var bExpandedRowsDialog = sFragment === DialogContracts.FRAGMENTS.CHECKS_EXPANDED
                || sFragment === DialogContracts.FRAGMENTS.BARRIERS_EXPANDED;
            if (!sFragment) {
                return Promise.resolve(null);
            }
            return DialogOrchestrator.ensure(this, sId, {
                dialogId: bExpandedRowsDialog ? "expandedRowsDialog" : undefined,
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
            return WorkflowContracts.isEditableMode(
                ModelStateRuntime.read(this, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ)
            );
        },

        _showToast: function (sTextKey) {
            return FeedbackCoordinator.showToast(this, sTextKey, [], "info");
        },

        _applyLayoutState: function (sLayout, mOptions) {
            DetailViewBehavior.applyLayoutState(this, sLayout, mOptions);
        },

        _currentRootId: function () {
            return ModelStateRuntime.read(this, STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, "")
                || ModelStateRuntime.read(this, STATE_MODEL, ModelPathContracts.SELECTED_ID, "")
                || "";
        },

        _applyDetailFieldChange: function (oEvent, mOptions) {
            return DetailSelectedFieldRuntime.applyDetailFieldChange(this, oEvent, mOptions, {
                isDirtyTrackMode: function () {
                    return isDirtyTrackMode(this);
                }.bind(this)
            });
        },

        _resolveRowInput: function (oEvent) {
            return DetailSelectedFieldRuntime.resolveRowInput(oEvent);
        }
    });
});
