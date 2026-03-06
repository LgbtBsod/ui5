sap.ui.define([
    "sap_ui5/service/framework/EffectApplier",
    "sap_ui5/controller/support/DetailActionConstants",
    "sap_ui5/controller/support/DetailCommandPolicy",
    "sap_ui5/service/framework/ClipboardRuntime",
    "sap_ui5/util/CreateSentinel",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (EffectApplier, DetailActionConstants, DetailCommandPolicy, ClipboardRuntime, CreateSentinel, ControllerModelWriteSupport) {
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

    return {
        onToggleEdit: function (oEvent) {
            return DetailCommandPolicy.enterEdit(this, { state: !!oEvent.getParameter("state"), rootId: this._currentRootId() }).finally(function () {
                this._scheduleAttachmentDropZoneBind();
            }.bind(this));
        },

        onSaveDetail: function () {
            var mBusyStart = {};
            if (ControllerModelWriteSupport.get(this, "state", "/isBusy") ||
                ControllerModelWriteSupport.get(this, "state", STATE_PATHS.SAVE_IN_FLIGHT)) {
                return Promise.resolve(false);
            }
            mBusyStart[STATE_PATHS.SAVE_IN_FLIGHT] = true;
            mBusyStart["/isBusy"] = true;
            ControllerModelWriteSupport.setMany(this, "state", mBusyStart);
            return DetailCommandPolicy.save(this, { rootId: this._currentRootId() }).finally(function () {
                ControllerModelWriteSupport.set(this, "state", STATE_PATHS.SAVE_IN_FLIGHT, false);
                if (ControllerModelWriteSupport.get(this, "state", "/isBusy")) {
                    ControllerModelWriteSupport.set(this, "state", "/isBusy", false);
                }
            }.bind(this));
        },

        onCloseDetail: function () {
            this._setDeleteChecklistConfirmArmed(false);
            DetailCommandPolicy.close(this, { rootId: this._currentRootId() });
        },

        onArmDeleteChecklist: function () {
            var bCurrent;
            if (ControllerModelWriteSupport.get(this, "state", "/isBusy") ||
                ControllerModelWriteSupport.get(this, "state", "/lockOperationPending")) {
                return Promise.resolve(false);
            }
            bCurrent = !!ControllerModelWriteSupport.get(this, "view", "/deleteChecklistConfirmArmed");
            if (!ControllerModelWriteSupport.set(this, "view", "/deleteChecklistConfirmArmed", !bCurrent)) {
                return Promise.resolve(false);
            }
            return Promise.resolve(true);
        },

        onConfirmDeleteChecklist: function () {
            var bArmed = !!ControllerModelWriteSupport.get(this, "view", "/deleteChecklistConfirmArmed");
            if (!bArmed || ControllerModelWriteSupport.get(this, "state", "/isBusy") ||
                ControllerModelWriteSupport.get(this, "state", "/lockOperationPending")) {
                return Promise.resolve(false);
            }
            return EffectApplier.promptWarning(
                this.getResourceBundle().getText("deleteChecklistConfirmText"),
                [EffectApplier.actions.DELETE, EffectApplier.actions.CANCEL],
                EffectApplier.actions.CANCEL
            ).then(function (sAction) {
                if (sAction !== EffectApplier.actions.DELETE) {
                    this._setDeleteChecklistConfirmArmed(false);
                    return false;
                }
                this._setDeleteChecklistConfirmArmed(false);
                return ControllerModelWriteSupport.withFlag(this, "state", "/isBusy", function () {
                    return DetailCommandPolicy.deleteChecklist(this, { rootId: this._currentRootId() });
                }.bind(this)).finally(function () {
                    this._setDeleteChecklistConfirmArmed(false);
                }.bind(this));
            }.bind(this));
        },

        onDeleteChecklist: function () {
            return this.onArmDeleteChecklist();
        },

        onCopyDetailLink: function () {
            var oState = this.getModel("state");
            var sId = oState && oState.getProperty("/activeObjectId");
            if (!sId || CreateSentinel.isCreateId(sId)) {
                return;
            }
            var sHash = this.getRouter().getURL("detail", { id: sId });
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
            var sLayout = String(oState.getProperty("/layout") || "TwoColumnsMidExpanded");
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
                    oObjectPage.scrollToSection(oTarget.getId(), 250, -28);
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
            iPinnedHeight = (document.querySelector(".detailControlExperienceCard.detailControlCardViewportPinned") || {}).offsetHeight || 0;
            if (oScrollHost && typeof oScrollHost.scrollTop === "number") {
                iHostTop = (oScrollHost.getBoundingClientRect && oScrollHost.getBoundingClientRect().top) || 0;
                iTargetTop = Math.max(
                    0,
                    Math.round(resolveOffsetTopWithinHost(oTargetNode, oScrollHost) - (iTopOffset + iPinnedHeight + 22 - iHostTop))
                );
                oScrollHost.scrollTo({ top: iTargetTop, behavior: "smooth" });
                return;
            }
            if (oObjectPage && typeof oObjectPage.scrollToSection === "function") {
                oObjectPage.scrollToSection(oTarget.getId(), 250, -(iTopOffset + iPinnedHeight + 22));
                return;
            }
            if (oTargetNode && typeof oTargetNode.scrollIntoView === "function") {
                oTargetNode.scrollIntoView({ behavior: "smooth", block: "start" });
            }
        },

        onCancelEditFromDetail: function () {
            DetailCommandPolicy.discardChanges(this, { rootId: this._currentRootId() });
        },

        onValidateChecklist: function () {
            this._recomputeValidationSummary("manualValidate", true);
            return DetailCommandPolicy.validate(this, { rootId: this._currentRootId() }).then(function (oResult) {
                this._recomputeValidationSummary("validateResult", true);
                if (this.getModel("state").getProperty(STATE_PATHS.VALIDATION_SUMMARY + "/hasErrors")) {
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
            DetailCommandPolicy.changeStatus(this, {
                rootId: this._currentRootId(),
                status: (oSrc && (oSrc.data("status") || oSrc.data("targetStatus"))) || ""
            });
        }
    };
});
