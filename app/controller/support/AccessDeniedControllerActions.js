sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/AccessPayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService"
], function (CtxFactory, DetailAuthorizationSupport, AccessPayload, ControllerRouteRuntime, ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService) {
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
        return AccessPayload.buildDeniedViewState(oGuard, sRootId);
    }

    function readCurrentUserId(oController) {
        var oStateModel = ModelStateRuntime.model(oController, "state");
        return String(ModelStateRuntime.readOnModel(oStateModel, "/currentUser/uname", "") || "").trim();
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
            return CtxFactory.buildCtx(this, {});
        },

        _renderGuardState: function (oGuard, sRootId) {
            ControllerViewStateRuntime.replace(this, function () {
                return normalizeGuard(oGuard, sRootId);
            });
        },

        _readCachedGuard: function (sRootId) {
            var oGuard = ModelStateRuntime.read(this, "state", "/detailAccessGuard", {}) || {};
            var sGuardRootId = String(oGuard.rootId || "").trim();
            var sGuardUserId = String(oGuard.userId || "").trim();
            var sCurrentUserId = readCurrentUserId(this);

            if (sGuardRootId && sGuardRootId === String(sRootId || "").trim()) {
                if (sCurrentUserId && sGuardUserId && sCurrentUserId !== sGuardUserId) {
                    return null;
                }
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
                ModelStateRuntime.writeOnModel(oStateModel, "/detailAccessGuard", Object.assign(
                    AccessPayload.buildGuard(oPermission, sRootId, {
                        canView: false,
                        reasonCode: "NO_VIEW_PERMISSION"
                    }),
                    { checkedAt: new Date().toISOString() }
                ));
                this._renderGuardState(oPermission, sRootId);
            }.bind(this)).catch(function () {
                ControllerViewStateRuntime.replace(this, function () {
                    return AccessPayload.buildDeniedViewState(null, sRootId);
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
