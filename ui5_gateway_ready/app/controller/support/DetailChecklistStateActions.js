sap.ui.define([
    "checklist/app/service/framework/LockSaveFlowOrchestrator",
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/service/framework/ClipboardRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/RootIdRuntime",
    "checklist/app/util/CreateSentinel"
], function (LockSaveFlowOrchestrator, DetailActionConstants, DetailCommandPolicy, ClipboardRuntime, LayoutStateRuntime, ModelStateRuntime, NavigationIntentService, RootIdRuntime, CreateSentinel) {
    "use strict";

    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

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

    return {
        onToggleEdit: function (oEvent) {
            return LockSaveFlowOrchestrator.toggleEdit(this, oEvent);
        },

        onSaveDetail: function () {
            return LockSaveFlowOrchestrator.save(this, {
                saveInFlightPath: STATE_PATHS.SAVE_IN_FLIGHT
            });
        },

        onCloseDetail: function () {
            return LockSaveFlowOrchestrator.close(this);
        },

        onArmDeleteChecklist: function () {
            return LockSaveFlowOrchestrator.armDelete(this);
        },

        onConfirmDeleteChecklist: function () {
            return LockSaveFlowOrchestrator.confirmDelete(this);
        },

        onDeleteChecklist: function () {
            return this.onArmDeleteChecklist();
        },

        onCopyDetailLink: function () {
            var oState = this.getModel("state");
            var sId = RootIdRuntime.resolveActiveFromStateModel(oState);
            if (!sId || CreateSentinel.isCreateId(sId)) {
                return;
            }
            var sHash = NavigationIntentService.buildDetailHash(this, sId);
            var sUrl = window.location.origin + window.location.pathname + "#" + sHash;
            ClipboardRuntime.writeText(sUrl).then(function (bCopied) {
                this._showToast(bCopied ? "detailLinkCopied" : "detailLinkCopyFailed");
            }.bind(this));
        },

        onToggleDetailFullscreen: function () {
            var oState = this.getModel("state");
            if (!oState || !oState.getProperty || !oState.setProperty) {
                return;
            }
            var sLayout = LayoutStateRuntime.readLayout(oState, "TwoColumnsMidExpanded");
            var sNextLayout = sLayout === "MidColumnFullScreen" ? "TwoColumnsMidExpanded" : "MidColumnFullScreen";
            this._applyLayoutState(sNextLayout);
        },

        onJumpToDetailSection: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sTargetId = String((oSource && oSource.data && oSource.data("targetSection")) || "").trim();
            var oObjectPage = this.byId("detailObjectPage");
            var oTarget = sTargetId && this.byId(sTargetId);
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
                    oObjectPage.scrollToSection(oTarget.getId(), 250, -(resolvePinnedRailHeight(this) + 22));
                }
                return;
            }
            oTargetNode = oDomRef.querySelector(".detailSectionCard")
                || oDomRef.querySelector(".detailSectionBody")
                || oDomRef.querySelector(".sapUxAPBlockContainer > *")
                || oDomRef.querySelector(".sapMListTbl")
                || oDomRef.querySelector(".sapUiTable")
                || oDomRef;
            oScrollHost = (typeof this._resolveDetailScrollHost === "function" && this._resolveDetailScrollHost())
                || document.querySelector(".sapUxAPObjectPageWrapper")
                || document.querySelector(".sapUxAPObjectPageScroll")
                || document.querySelector(".sapUxAPObjectPageContainer")
                || document.scrollingElement;
            iTopOffset = parseFloat((window.getComputedStyle(document.documentElement).getPropertyValue("--app-shell-offset") || "").replace("px", ""));
            if (!Number.isFinite(iTopOffset)) {
                iTopOffset = 88;
            }
            iPinnedHeight = resolvePinnedRailHeight(this);
            if (oScrollHost && typeof oScrollHost.scrollTop === "number") {
                iHostTop = (oScrollHost.getBoundingClientRect && oScrollHost.getBoundingClientRect().top) || 0;
                iTargetTop = Math.max(
                    0,
                    Math.round(resolveOffsetTopWithinHost(oTargetNode, oScrollHost) - (iTopOffset + iPinnedHeight + 16 - iHostTop))
                );
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

        onCancelEditFromDetail: function () {
            DetailCommandPolicy.discardChanges(this, RootIdRuntime.withCurrentRootId(this));
        },

        onValidateChecklist: function () {
            this._recomputeValidationSummary("manualValidate", true);
            return DetailCommandPolicy.validate(this, RootIdRuntime.withCurrentRootId(this)).then(function (oResult) {
                this._recomputeValidationSummary("validateResult", true);
                if (ModelStateRuntime.read(this, "state", STATE_PATHS.VALIDATION_SUMMARY + "/hasErrors", false)) {
                    this._focusFirstInvalidField();
                }
                return oResult;
            }.bind(this));
        },

        onFocusFirstInvalid: function () {
            this._recomputeValidationSummary("summaryFocus", true);
            this._focusFirstInvalidField();
        },

        onChangeChecklistStatus: function (oEvent) {
            var oSrc = oEvent && oEvent.getSource && oEvent.getSource();
            DetailCommandPolicy.changeStatus(this, RootIdRuntime.withCurrentRootId(this, {
                status: (oSrc && (oSrc.data("status") || oSrc.data("targetStatus"))) || ""
            }));
        }
    };
});
