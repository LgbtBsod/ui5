sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap_ui5/controller/support/ControllerResourceCleanup",
    "sap_ui5/service/domain/detail/DetailFacade",
    "sap_ui5/service/framework/CtxFactory",
    "sap_ui5/service/framework/FacadeCommandContract",
    "sap_ui5/controller/base/ControllerTextRuntime",
    "sap_ui5/controller/support/ControllerModelWriteSupport",
    "sap_ui5/controller/support/AttachmentUploadSupport",
    "sap_ui5/controller/support/DetailAccessViewState",
    "sap_ui5/controller/support/DetailInfoCardLayoutSupport"
], function (
    JSONModel,
    ControllerResourceCleanup,
    DetailFacade,
    CtxFactory,
    FacadeCommandContract,
    ControllerTextRuntime,
    ControllerModelWriteSupport,
    AttachmentUploadSupport,
    DetailAccessViewState,
    DetailInfoCardLayoutSupport
) {
    "use strict";

    function parseInitialDetailHash() {
        var sHash = String((typeof window !== "undefined" && window.location && window.location.hash) || "").trim();
        var oMatch;
        if (!sHash) {
            return null;
        }
        sHash = sHash.replace(/^#\/?/, "");
        oMatch = /^checklist\/([^\/?#]+)(?:\/([^\/?#]+))?$/i.exec(sHash);
        if (!oMatch) {
            return null;
        }
        return {
            id: decodeURIComponent(oMatch[1] || ""),
            layout: decodeURIComponent(oMatch[2] || "")
        };
    }

    function buildInitialViewState(oController) {
        var fnText = function (sKey, sFallback) {
            return ControllerTextRuntime.getText(oController, sKey, [], sFallback || sKey);
        };
        return {
            detailSkeletonBusy: false,
            attachmentBusy: false,
            checksBusy: false,
            barriersBusy: false,
            checksExpandedBusy: false,
            barriersExpandedBusy: false,
            locationVhBusy: false,
            attachmentCategoryKey: "GEN",
            observerSuggestions: [],
            observedSuggestions: [],
            observerInputValue: "",
            observedInputValue: "",
            personSuggestHint: "",
            locationVhHint: "",
            narrowDetailViewport: false,
            deleteChecklistConfirmArmed: false,
            accessState: DetailAccessViewState.createDefaultState(""),
            validationShown: false,
            validationMissing: {},
            infoCards: DetailInfoCardLayoutSupport.resolveCards(oController, [
                { key: "datetime", title: fnText("dateTimeBlockLabel", "Date & Time"), pinned: true },
                { key: "observer", title: fnText("observerLabel", "Observer"), pinned: true },
                { key: "observed", title: fnText("observedLabel", "Observed"), pinned: true },
                { key: "equipment", title: fnText("equipmentLabel", "Equipment"), pinned: false },
                { key: "location", title: fnText("locationLabel", "Location"), pinned: false },
                { key: "lpc", title: fnText("lpcLabel", "LPC"), pinned: false },
                { key: "profession", title: fnText("professionLabel", "Profession"), pinned: false }
            ])
        };
    }

    function bindStateValidationModel(oController) {
        oController._oStateValidationModel = oController.getModel("state") || null;
        if (!oController._oStateValidationModel || !oController._oStateValidationModel.attachPropertyChange) {
            return;
        }
        oController._fnStateValidationChange = function (oEvent) {
            var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
            var oViewModel;
            if (sPath === "/requiredFields") {
                oController._recomputeValidationSummary("requiredFieldsChanged", false);
                return;
            }
            if (sPath !== "/mode" && sPath !== "/activeObjectId" && sPath !== "/selectedId") {
                return;
            }
            oViewModel = oController.getModel("view");
            if (oViewModel) {
                ControllerModelWriteSupport.set(oController, "view", "/deleteChecklistConfirmArmed", false);
            }
        };
        oController._oStateValidationModel.attachPropertyChange(oController._fnStateValidationChange, oController);
    }

    return {
        onInit: function () {
            this._facade = new DetailFacade();
            this._mLazyDialogs = {};
            this._mDialogReturnFocus = {};
            this._iAttachmentDropZoneBindTimer = null;
            this._iLocationVhSearchTimer = null;
            this._iLocationVhTableSyncTimer = null;
            this._oDetailScrollHost = null;
            this._fnDetailScrollSync = null;
            this._fnDetailResizeSync = null;
            this._oAdaptiveViewportResizeObserver = null;
            this._fnAdaptiveViewportSync = null;
            this.attachRouteMatched("detail", this._onDetailMatched);
            this.attachRouteMatched("detailLayout", this._onDetailMatched);

            this.setModel(new JSONModel(buildInitialViewState(this)), "view");

            this._oSelectedModel = this.getModel("selected") || null;
            if (this._oSelectedModel && this._oSelectedModel.attachPropertyChange) {
                this._oSelectedModel.attachPropertyChange(this._onSelectedChecklistChanged, this);
            }
            bindStateValidationModel(this);
        },

        onAfterRendering: function () {
            this._scheduleAttachmentDropZoneBind();
            this._bindDetailEditSwitchKeyboardFallback();
            this._bindAdaptiveDetailViewport();
            this._bindViewportPinnedControlRail();
            this._replayInitialDetailRouteIfNeeded();
        },

        onExit: function () {
            if (this._oSelectedModel && this._oSelectedModel.detachPropertyChange) {
                this._oSelectedModel.detachPropertyChange(this._onSelectedChecklistChanged, this);
            }
            if (this._oStateValidationModel && this._fnStateValidationChange && this._oStateValidationModel.detachPropertyChange) {
                this._oStateValidationModel.detachPropertyChange(this._fnStateValidationChange, this);
            }
            if (this.detachAllRouteMatched) {
                this.detachAllRouteMatched();
            }
            if (this._iAttachmentDropZoneBindTimer) {
                clearTimeout(this._iAttachmentDropZoneBindTimer);
                this._iAttachmentDropZoneBindTimer = null;
            }
            this._clearLocationValueHelpSearchTimer();
            if (this._iLocationVhTableSyncTimer) {
                clearTimeout(this._iLocationVhTableSyncTimer);
                this._iLocationVhTableSyncTimer = null;
            }
            this._unbindViewportPinnedControlRail();
            AttachmentUploadSupport.unbindDropZone(this);
            ControllerResourceCleanup.destroyMapEntries(this._mLazyDialogs);
            this._oSelectedModel = null;
            this._mLazyDialogs = null;
            this._mDialogReturnFocus = null;
            this._oAdaptiveViewportResizeObserver = null;
            this._fnAdaptiveViewportSync = null;
            this._oStateValidationModel = null;
            this._fnStateValidationChange = null;
        },

        _ctx: function () {
            return CtxFactory.buildCtx(this, {});
        },

        _run: function (sMethod, mInput) {
            var sCommand = FacadeCommandContract.normalizeDetailMethod(sMethod);
            var oPayload = FacadeCommandContract.normalizeDetailPayload(sCommand, mInput);
            return this.executeFacadeMethod(this._facade, sCommand, oPayload, this._ctx());
        },

        _replayInitialDetailRouteIfNeeded: function () {
            var oStateModel = this.getModel("state");
            var oSelectedModel = this.getModel("selected");
            var oParsedRoute = parseInitialDetailHash();
            var sCurrentRouteName = String((oStateModel && oStateModel.getProperty && oStateModel.getProperty("/currentRouteName")) || "search").trim() || "search";
            var sLoadedRootId = String((oSelectedModel && oSelectedModel.getProperty && oSelectedModel.getProperty("/root/id")) || "").trim();

            if (!oParsedRoute || !oParsedRoute.id || sLoadedRootId || sCurrentRouteName !== "search") {
                return;
            }
            this._onDetailMatched({
                getParameter: function (sName) {
                    if (sName === "arguments") {
                        return {
                            id: oParsedRoute.id,
                            layout: oParsedRoute.layout
                        };
                    }
                    if (sName === "name") {
                        return oParsedRoute.layout ? "detailLayout" : "detail";
                    }
                    return null;
                }
            });
        }
    };
});

