sap.ui.define([
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/domain/detail/DetailAuthorizationSupport",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime"
], function (ControllerCtxRuntime, DetailAuthorizationSupport, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime) {
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
        var oStateModel = ModelStateRuntime.model(oController, "state");
        var oSelectedModel = ModelStateRuntime.model(oController, "selected");
        var oUiStateModel = ModelStateRuntime.model(oController, "uiState");

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
            ControllerViewStateRuntime.initModel(this, function () {
                return buildInitialViewState("");
            });
            ControllerRouteRuntime.attachMatched(this, [
                { name: "accessDenied", handler: this._onAccessDeniedMatched }
            ]);
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
        },

        _ctx: function () {
            return ControllerCtxRuntime.buildDefault(this);
        },

        _renderGuardState: function (oGuard, sRootId) {
            ControllerViewStateRuntime.replace(this, function () {
                return normalizeGuard(oGuard, sRootId);
            });
        },

        _readCachedGuard: function (sRootId) {
            var oStateModel = ModelStateRuntime.model(this, "state");
            var oGuard = (oStateModel && oStateModel.getProperty && oStateModel.getProperty("/detailAccessGuard")) || {};
            var sGuardRootId = String(oGuard.rootId || "").trim();

            if (sGuardRootId && sGuardRootId === String(sRootId || "").trim()) {
                return oGuard;
            }
            return null;
        },

        _refreshAccessState: function (sRootId) {
            var oStateModel = ModelStateRuntime.model(this, "state");

            clearDetailRuntimeState(this);
            ControllerViewStateRuntime.replace(this, function () {
                return {
                    busy: true,
                    rootId: String(sRootId || "").trim(),
                    reasonCode: "",
                    message: ""
                };
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
                ControllerViewStateRuntime.replace(this, function () {
                    return {
                        busy: false,
                        rootId: String(sRootId || "").trim(),
                        reasonCode: "NO_VIEW_PERMISSION",
                        message: ""
                    };
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
            var sRootId = String(ControllerViewStateRuntime.get(this, "/rootId", "") || "").trim();
            if (!sRootId) {
                return Promise.resolve();
            }
            return this._refreshAccessState(sRootId);
        }
    };
});
