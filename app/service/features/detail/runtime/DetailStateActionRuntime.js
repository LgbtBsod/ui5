sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ClipboardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailResetRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDecisionRuntime"
], function (
    ClipboardRuntime,
    LayoutStateRuntime,
    ControllerModelRuntime,
    ModelStateRuntime,
    DetailResetRuntime,
    NavigationIntentService,
    RootIdRuntime,
    UiDecisionCoordinator,
    WorkflowCoordinator,
    StatePaths,
    NavigationContracts,
    ModelContracts,
    OperationSourceContracts,
    JsRuntime,
    ModelPathContracts,
    WorkflowDecisionRuntime
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var SHELL_MODEL = MODELS.SHELL;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function resolvePinnedRailHeight(oController) {
        var oStickyHost = oController && oController.byId && oController.byId("detailControlPinnedDock");
        var oStickyDom = oStickyHost && typeof oStickyHost[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oStickyHost[METHODS.GET_DOM_REF]();
        return Math.round((oStickyDom && oStickyDom.offsetHeight) || 0);
    }

    function resetDeleteChecklistConfirmArmed(oController) {
        ModelStateRuntime.write(oController, VIEW_MODEL, "/deleteChecklistConfirmArmed", false);
    }

    var workflowActions = {
        armDelete: function (oController) {
            var bCurrent;
            if (ModelStateRuntime.any(oController, STATE_MODEL, [StatePaths.UI_BUSY_DETAIL, "/lockOperationPending"])) {
                return Promise.resolve(false);
            }
            bCurrent = !!ModelStateRuntime.read(oController, VIEW_MODEL, "/deleteChecklistConfirmArmed", false);
            ModelStateRuntime.write(oController, VIEW_MODEL, "/deleteChecklistConfirmArmed", !bCurrent);
            return Promise.resolve(true);
        },
        close: function (oController, mHooks) {
            resetDeleteChecklistConfirmArmed(oController);
            return WorkflowCoordinator.confirmUnsavedAndHandle(oController, function () {
                return mHooks.saveDetail();
            }).then(function (sDecision) {
                if (sDecision === WorkflowDecisionRuntime.RESULTS.CANCEL || sDecision === WorkflowDecisionRuntime.RESULTS.SAVE_FAILED) {
                    return false;
                }
                if (sDecision === WorkflowDecisionRuntime.RESULTS.DISCARD) {
                    DetailResetRuntime.resetDetailWorkflowState(oController, {
                        [ModelPathContracts.SELECTED_ID]: "",
                        [ModelPathContracts.ACTIVE_OBJECT_ID]: ""
                    });
                    ModelStateRuntime.write(oController, SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN);
                    DetailResetRuntime.resetDetailRuntimeData(oController);
                    NavigationIntentService.navigateToSearch(oController);
                    return true;
                }
                return mHooks.closeDetail({ intent: DETAIL_SOURCES.CLOSE });
            });
        },
        confirmDelete: function (oController, mHooks) {
            return UiDecisionCoordinator.confirmDeleteChecklist({
                controller: oController,
                armed: !!ModelStateRuntime.read(oController, VIEW_MODEL, "/deleteChecklistConfirmArmed", false),
                busy: ModelStateRuntime.any(oController, STATE_MODEL, [StatePaths.UI_BUSY_DETAIL, "/lockOperationPending"]),
                onReset: function () { resetDeleteChecklistConfirmArmed(oController); },
                onConfirm: function () {
                    return ModelStateRuntime.withFlag(oController, STATE_MODEL, StatePaths.UI_BUSY_DETAIL, function () {
                        return mHooks.deleteChecklist();
                    }, true, false);
                }
            });
        },
        save: function (oController, mHooks, mOptions) {
            var sSaveInFlightPath = (mOptions && mOptions.saveInFlightPath) || "/saveInFlight";
            if (ModelStateRuntime.any(oController, STATE_MODEL, [StatePaths.UI_BUSY_DETAIL, sSaveInFlightPath])) {
                return Promise.resolve(false);
            }
            return ModelStateRuntime.withFlags(oController, STATE_MODEL, [sSaveInFlightPath, StatePaths.UI_BUSY_DETAIL], function () {
                return mHooks.saveDetail();
            });
        },
        toggleEdit: function (oController, oEvent, mHooks) {
            return Promise.resolve(mHooks.enterEdit({
                state: !!(oEvent && typeof oEvent.getParameter === TYPE_FUNCTION && oEvent.getParameter("state"))
            })).finally(function () {
                if (typeof oController._scheduleAttachmentDropZoneBind === TYPE_FUNCTION) {
                    oController._scheduleAttachmentDropZoneBind();
                }
            });
        }
    };

    var navigationActions = {
        copyDetailLink: function (oController, mHooks) {
            var oState = ControllerModelRuntime.state(oController);
            var sId = RootIdRuntime.resolveActiveFromStateModel(oState);
            var sHash;
            var sUrl;
            if (!sId || mHooks.isCreateId(sId)) {
                return;
            }
            sHash = NavigationIntentService.buildDetailHash(oController, sId);
            sUrl = window.location.origin + window.location.pathname + "#" + sHash;
            ClipboardRuntime.writeText(sUrl).then(function (bCopied) {
                mHooks.showToast(bCopied ? "detailLinkCopied" : "detailLinkCopyFailed");
            });
        },
        jumpToDetailSection: function (oController, oEvent) {
            var oSource = oEvent && typeof oEvent.getSource === TYPE_FUNCTION && oEvent.getSource();
            var sTargetId = String((oSource && typeof oSource.data === TYPE_FUNCTION && oSource.data("targetSection")) || "").trim();
            var oObjectPage = oController.byId("detailObjectPage");
            var oTarget = sTargetId && oController.byId(sTargetId);
            var oTargetDom;
            if (!sTargetId || !oTarget) {
                return;
            }
            if (oObjectPage && typeof oObjectPage.scrollToSection === TYPE_FUNCTION && typeof oTarget.getId === TYPE_FUNCTION) {
                oObjectPage.scrollToSection(oTarget.getId(), 250, -(resolvePinnedRailHeight(oController) + 22));
                return;
            }
            oTargetDom = typeof oTarget[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oTarget[METHODS.GET_DOM_REF]();
            if (oTargetDom && typeof oTargetDom.scrollIntoView === TYPE_FUNCTION) {
                oTargetDom.scrollIntoView({ behavior: "smooth", block: "start" });
            }
        },
        toggleFullscreen: function (oController, mHooks) {
            var oState = ControllerModelRuntime.state(oController);
            var oShell = ControllerModelRuntime.shell(oController);
            var sLayout;
            var sNextLayout;
            if (!oState || !oShell || typeof oState[METHODS.GET_PROPERTY] !== TYPE_FUNCTION || typeof oState[METHODS.SET_PROPERTY] !== TYPE_FUNCTION) {
                return;
            }
            sLayout = LayoutStateRuntime.readLayout(oShell, NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED);
            sNextLayout = sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
                ? NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED
                : NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
            mHooks.applyLayoutState(sNextLayout);
        }
    };

    return {
        workflowActions: workflowActions,
        navigationActions: navigationActions,
        armDelete: workflowActions.armDelete,
        close: workflowActions.close,
        confirmDelete: workflowActions.confirmDelete,
        save: workflowActions.save,
        toggleEdit: workflowActions.toggleEdit,
        copyDetailLink: navigationActions.copyDetailLink,
        jumpToDetailSection: navigationActions.jumpToDetailSection,
        toggleFullscreen: navigationActions.toggleFullscreen
    };
});
