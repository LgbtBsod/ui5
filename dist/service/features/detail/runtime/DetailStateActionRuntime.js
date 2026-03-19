sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ClipboardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts"
], function (ClipboardRuntime, LayoutStateRuntime, ControllerModelRuntime, ModelStateRuntime, NavigationIntentService, RootIdRuntime, UiDecisionCoordinator, WorkflowCoordinator, StatePaths, NavigationContracts, ModelContracts, OperationSourceContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;

    function resolveOffsetTopWithinHost(oNode, oHost) {
        var iTop = 0, oCurrent = oNode;
        while (oCurrent && oCurrent !== oHost) {
            iTop += oCurrent.offsetTop || 0;
            oCurrent = oCurrent.offsetParent;
        }
        return iTop;
    }

    function resolvePinnedRailHeight(oController) {
        var oStickyHost = oController && oController.byId && oController.byId("detailControlPinnedDock");
        var oStickyDom = oStickyHost && oStickyHost.getDomRef && oStickyHost.getDomRef();
        return Math.round((oStickyDom && oStickyDom.offsetHeight) || 0);
    }

    function resetDeleteChecklistConfirmArmed(oController) {
        ModelStateRuntime.write(oController, VIEW_MODEL, "/deleteChecklistConfirmArmed", false);
    }

    return {
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
                if (sDecision === "CANCEL" || sDecision === "SAVE_FAILED") {
                    return false;
                }
                if (sDecision === "DISCARD") {
                    ModelStateRuntime.resetDetailWorkflowState(oController, {
                        "/layout": NavigationContracts.LAYOUTS.ONE_COLUMN,
                        "/selectedId": "",
                        "/activeObjectId": ""
                    });
                    ModelStateRuntime.resetDetailRuntimeData(oController);
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
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sTargetId = String((oSource && oSource.data && oSource.data("targetSection")) || "").trim();
            var oObjectPage = oController.byId("detailObjectPage");
            var oTarget = sTargetId && oController.byId(sTargetId);
            var oDomRef;
            var oTargetNode;
            var oScrollHost;
            var iTopOffset;
            var iPinnedHeight;
            var iTargetTop;
            var iHostTop;
            if (!sTargetId || !oTarget) {
                return;
            }
            oDomRef = oTarget.getDomRef && oTarget.getDomRef();
            if (!oDomRef) {
                if (oObjectPage && typeof oObjectPage.scrollToSection === "function") {
                    oObjectPage.scrollToSection(oTarget.getId(), 250, -(resolvePinnedRailHeight(oController) + 22));
                }
                return;
            }
            oTargetNode = oDomRef.querySelector(".detailSectionCard")
                || oDomRef.querySelector(".detailSectionBody")
                || oDomRef.querySelector(".sapUxAPBlockContainer > *")
                || oDomRef.querySelector(".sapMListTbl")
                || oDomRef.querySelector(".sapUiTable")
                || oDomRef;
            oScrollHost = (typeof oController._resolveDetailScrollHost === "function" && oController._resolveDetailScrollHost())
                || document.querySelector(".sapUxAPObjectPageWrapper")
                || document.querySelector(".sapUxAPObjectPageScroll")
                || document.querySelector(".sapUxAPObjectPageContainer")
                || document.scrollingElement;
            iTopOffset = parseFloat((window.getComputedStyle(document.documentElement).getPropertyValue("--app-shell-offset") || "").replace("px", ""));
            if (!Number.isFinite(iTopOffset)) {
                iTopOffset = 88;
            }
            iPinnedHeight = resolvePinnedRailHeight(oController);
            if (oScrollHost && typeof oScrollHost.scrollTop === "number") {
                iHostTop = (oScrollHost.getBoundingClientRect && oScrollHost.getBoundingClientRect().top) || 0;
                iTargetTop = Math.max(0, Math.round(resolveOffsetTopWithinHost(oTargetNode, oScrollHost) - (iTopOffset + iPinnedHeight + 16 - iHostTop)));
                oScrollHost.scrollTo({ top: iTargetTop, behavior: "smooth" });
                return;
            }
            if (oObjectPage && typeof oObjectPage.scrollToSection === "function") {
                oObjectPage.scrollToSection(oTarget.getId(), 250, -(iTopOffset + iPinnedHeight + 16));
                return;
            }
            if (oTargetNode && typeof oTargetNode.scrollIntoView === "function") {
                oTargetNode.scrollIntoView({ behavior: "smooth", block: "start" });
            }
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
                state: !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"))
            })).finally(function () {
                if (typeof oController._scheduleAttachmentDropZoneBind === "function") {
                    oController._scheduleAttachmentDropZoneBind();
                }
            });
        },
        toggleFullscreen: function (oController, mHooks) {
            var oState = ControllerModelRuntime.state(oController);
            var sLayout;
            var sNextLayout;
            if (!oState || !oState.getProperty || !oState.setProperty) {
                return;
            }
            sLayout = LayoutStateRuntime.readLayout(oState, NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED);
            sNextLayout = sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN ? NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED : NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
            mHooks.applyLayoutState(sNextLayout);
        }
    };
});
