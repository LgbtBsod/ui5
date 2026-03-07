sap.ui.define([
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/domain/detail/DetailAuthorizationSupport",
    "checklist/app/service/framework/ControllerRouteRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/NavigationIntentService"
], function (ControllerCtxRuntime, DetailAuthorizationSupport, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService) {
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
        ModelStateRuntime.resetDetailWorkflowState(oController);
        ModelStateRuntime.resetDetailRuntimeData(oController);
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
            var oGuard = ModelStateRuntime.read(this, "state", "/detailAccessGuard", {}) || {};
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
                    NavigationIntentService.navigateToDetail(this, sRootId);
                    return;
                }
                ModelStateRuntime.writeOnModel(oStateModel, "/detailAccessGuard", {
                    rootId: String((oPermission && oPermission.rootId) || sRootId || "").trim(),
                    userId: String((oPermission && oPermission.userId) || "").trim(),
                    canView: false,
                    canEdit: !!(oPermission && oPermission.canEdit),
                    canDelete: !!(oPermission && oPermission.canDelete),
                    reasonCode: String((oPermission && oPermission.reasonCode) || "NO_VIEW_PERMISSION").trim(),
                    message: String((oPermission && oPermission.message) || "").trim(),
                    checkedAt: new Date().toISOString()
                });
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
            NavigationIntentService.navigateToSearch(this);
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
