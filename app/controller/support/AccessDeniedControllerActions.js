sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap_ui5/service/framework/CtxFactory",
    "sap_ui5/service/domain/detail/DetailAuthorizationSupport"
], function (JSONModel, CtxFactory, DetailAuthorizationSupport) {
    "use strict";

    function buildInitialViewState(sRootId) {
        return {
            busy: false,
            rootId: String(sRootId || "").trim(),
            reasonCode: "",
            message: ""
        };
    }

    function normalizeGuard(oGuard, sRootId) {
        var oResolved = oGuard || {};

        return {
            busy: false,
            rootId: String(oResolved.rootId || sRootId || "").trim(),
            reasonCode: String(oResolved.reasonCode || "NO_VIEW_PERMISSION").trim(),
            message: String(oResolved.message || "").trim()
        };
    }

    function clearDetailRuntimeState(oController) {
        var oStateModel = oController.getModel("state");
        var oSelectedModel = oController.getModel("selected");
        var oUiStateModel = oController.getModel("uiState");

        if (oStateModel && typeof oStateModel.setProperty === "function") {
            oStateModel.setProperty("/mode", "READ");
            oStateModel.setProperty("/lockOperationState", "IDLE");
            oStateModel.setProperty("/autosaveState", "IDLE");
            oStateModel.setProperty("/autosaveAt", null);
            oStateModel.setProperty("/autosaveEnabled", false);
            oStateModel.setProperty("/isDirty", false);
            oStateModel.setProperty("/activeObjectId", "");
        }
        if (oSelectedModel && typeof oSelectedModel.setData === "function") {
            oSelectedModel.setData({});
        }
        if (oUiStateModel && typeof oUiStateModel.setProperty === "function") {
            oUiStateModel.setProperty("/_detailSnapshot", {});
            oUiStateModel.setProperty("/_detailCurrent", {});
        }
    }

    return {
        onInit: function () {
            this.setModel(new JSONModel(buildInitialViewState("")), "view");
            this.attachRouteMatched("accessDenied", this._onAccessDeniedMatched);
        },

        onExit: function () {
            if (this.detachAllRouteMatched) {
                this.detachAllRouteMatched();
            }
        },

        _ctx: function () {
            return CtxFactory.buildCtx(this, {});
        },

        _renderGuardState: function (oGuard, sRootId) {
            this.getModel("view").setData(normalizeGuard(oGuard, sRootId));
        },

        _readCachedGuard: function (sRootId) {
            var oStateModel = this.getModel("state");
            var oGuard = (oStateModel && oStateModel.getProperty && oStateModel.getProperty("/detailAccessGuard")) || {};
            var sGuardRootId = String(oGuard.rootId || "").trim();

            if (sGuardRootId && sGuardRootId === String(sRootId || "").trim()) {
                return oGuard;
            }
            return null;
        },

        _refreshAccessState: function (sRootId) {
            var oStateModel = this.getModel("state");

            clearDetailRuntimeState(this);
            this.getModel("view").setData({
                busy: true,
                rootId: String(sRootId || "").trim(),
                reasonCode: "",
                message: ""
            });

            return DetailAuthorizationSupport.fetchPermission(this._ctx(), sRootId).then(function (oPermission) {
                if (oPermission && oPermission.canView) {
                    this.getRouter().navTo("detail", { id: sRootId }, false);
                    return;
                }
                if (oStateModel && typeof oStateModel.setProperty === "function") {
                    oStateModel.setProperty("/detailAccessGuard", {
                        rootId: String((oPermission && oPermission.rootId) || sRootId || "").trim(),
                        userId: String((oPermission && oPermission.userId) || "").trim(),
                        canView: false,
                        canEdit: !!(oPermission && oPermission.canEdit),
                        canDelete: !!(oPermission && oPermission.canDelete),
                        reasonCode: String((oPermission && oPermission.reasonCode) || "NO_VIEW_PERMISSION").trim(),
                        message: String((oPermission && oPermission.message) || "").trim(),
                        checkedAt: new Date().toISOString()
                    });
                }
                this._renderGuardState(oPermission, sRootId);
            }.bind(this)).catch(function () {
                this.getModel("view").setData({
                    busy: false,
                    rootId: String(sRootId || "").trim(),
                    reasonCode: "NO_VIEW_PERMISSION",
                    message: ""
                });
            }.bind(this));
        },

        _onAccessDeniedMatched: function (oEvent) {
            var mArgs = oEvent.getParameter("arguments") || {};
            var sRootId = String(mArgs.id || "").trim();
            var oCachedGuard = this._readCachedGuard(sRootId);

            clearDetailRuntimeState(this);
            if (oCachedGuard) {
                this._renderGuardState(oCachedGuard, sRootId);
                return Promise.resolve();
            }

            return this._refreshAccessState(sRootId);
        },

        onBackToSearch: function () {
            this.getRouter().navTo("search", {}, false);
        },

        onRetryAccessCheck: function () {
            var sRootId = String(this.getModel("view").getProperty("/rootId") || "").trim();
            if (!sRootId) {
                return Promise.resolve();
            }
            return this._refreshAccessState(sRootId);
        }
    };
});
