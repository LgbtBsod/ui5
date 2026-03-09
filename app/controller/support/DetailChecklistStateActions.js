sap.ui.define([
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/service/framework/ClipboardRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/service/framework/ControllerModelRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/RootIdRuntime",
    "checklist/app/service/framework/UiDecisionCoordinator",
    "checklist/app/util/CreateSentinel"
], function (DetailActionConstants, DetailCommandPolicy, ClipboardRuntime, LayoutStateRuntime, ControllerModelRuntime, ModelStateRuntime, NavigationIntentService, RootIdRuntime, UiDecisionCoordinator, CreateSentinel) {
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

    function resetDeleteChecklistConfirmArmed(oController) {
        ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", false);
    }

    function toggleEdit(oController, oEvent) {
        return Promise.resolve(DetailCommandPolicy.enterEdit(oController, RootIdRuntime.withCurrentRootId(oController, {
            state: !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"))
        }))).finally(function () {
            if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
                oController._scheduleAttachmentDropZoneBind();
            }
        });
    }

    function save(oController, mOptions) {
        var sSaveInFlightPath = (mOptions && mOptions.saveInFlightPath) || "/saveInFlight";
        if (ModelStateRuntime.any(oController, "state", ["/isBusy", sSaveInFlightPath])) {
            return Promise.resolve(false);
        }
        return ModelStateRuntime.withFlags(oController, "state", [sSaveInFlightPath, "/isBusy"], function () {
            return DetailCommandPolicy.save(oController, RootIdRuntime.withCurrentRootId(oController));
        });
    }

    function close(oController) {
        resetDeleteChecklistConfirmArmed(oController);
        return DetailCommandPolicy.close(oController, RootIdRuntime.withCurrentRootId(oController));
    }

    function armDelete(oController) {
        var bCurrent;
        if (ModelStateRuntime.any(oController, "state", ["/isBusy", "/lockOperationPending"])) {
            return Promise.resolve(false);
        }
        bCurrent = !!ModelStateRuntime.read(oController, "view", "/deleteChecklistConfirmArmed", false);
        ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", !bCurrent);
        return Promise.resolve(true);
    }

    function confirmDelete(oController) {
        return UiDecisionCoordinator.confirmDeleteChecklist({
            controller: oController,
            armed: !!ModelStateRuntime.read(oController, "view", "/deleteChecklistConfirmArmed", false),
            busy: ModelStateRuntime.any(oController, "state", ["/isBusy", "/lockOperationPending"]),
            onReset: function () {
                resetDeleteChecklistConfirmArmed(oController);
            },
            onConfirm: function () {
                return ModelStateRuntime.withFlag(oController, "state", "/isBusy", function () {
                    return DetailCommandPolicy.deleteChecklist(oController, RootIdRuntime.withCurrentRootId(oController));
                }, true, false);
            }
        });
    }

    return {
        onToggleEdit: function (oEvent) {
            return toggleEdit(this, oEvent);
        },

        onSaveDetail: function () {
            return save(this, {
                saveInFlightPath: STATE_PATHS.SAVE_IN_FLIGHT
            });
        },

        onCloseDetail: function () {
            return close(this);
        },

        onArmDeleteChecklist: function () {
            return armDelete(this);
        },

        onConfirmDeleteChecklist: function () {
            return confirmDelete(this);
        },

        onDeleteChecklist: function () {
            return this.onArmDeleteChecklist();
        },

        onCopyDetailLink: function () {
            var oState = ControllerModelRuntime.state(this);
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
            var oState = ControllerModelRuntime.state(this);
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
