sap.ui.define([
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeRules",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteHashResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (HashChanger, DebugLogger, RouteModeRules, RouteHashResolver, ModelStateRuntime, NavigationContracts, StatePaths) {
    "use strict";

    function debugLog(sEvent, oPayload) {
        DebugLogger.info("RouteModeCoordinator", sEvent, oPayload || {});
    }

    function normalizeId(vId) {
        var sId = String(vId || "").trim();
        return sId || null;
    }

    function resolveSelectedId(sRouteName, mArgs, oStateModel) {
        var sRoute = String(sRouteName || "");
        var sArgId = normalizeId(mArgs && mArgs.id);

        if (sRoute === NavigationContracts.ROUTES.SEARCH) {
            return null;
        }
        if (sRoute === NavigationContracts.ROUTES.ANALYTICS) {
            return normalizeId(
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "")
            );
        }
        if (NavigationContracts.isDetailRoute(sRoute) && sArgId) {
            return sArgId;
        }
        return normalizeId(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") ||
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, "")
        );
    }

    function resolveActiveObjectId(sRouteName, mArgs, oStateModel) {
        var sRoute = String(sRouteName || "");
        var sArgId = normalizeId(mArgs && mArgs.id);

        if (sRoute === NavigationContracts.ROUTES.SEARCH) {
            return null;
        }
        if (NavigationContracts.isDetailRoute(sRoute) && sArgId) {
            return sArgId;
        }
        return normalizeId(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") ||
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, "")
        );
    }

    function syncRouteState(oStateModel, sRouteName, mArgs) {
        var sPrevRouteName;
        var sNextSelectedId;
        var sNextActiveObjectId;
        var sPrevSelectedId;
        var sPrevActiveObjectId;
        var sNextRouteName;
        var bChanged = false;

        if (!oStateModel || typeof oStateModel.getProperty !== "function" || typeof oStateModel.setProperty !== "function") {
            return null;
        }

        sPrevSelectedId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, ""));
        sPrevActiveObjectId = normalizeId(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, ""));
        sPrevRouteName = String(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH
        ).trim() || NavigationContracts.ROUTES.SEARCH;
        sNextSelectedId = resolveSelectedId(sRouteName, mArgs, oStateModel);
        sNextActiveObjectId = resolveActiveObjectId(sRouteName, mArgs, oStateModel);
        sNextRouteName = String(sRouteName || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sPrevSelectedId !== sNextSelectedId) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.SELECTED_ID, sNextSelectedId);
            bChanged = true;
        }
        if (sPrevActiveObjectId !== sNextActiveObjectId) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, sNextActiveObjectId);
            bChanged = true;
        }
        if (sPrevRouteName !== sNextRouteName) {
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.CURRENT_ROUTE_NAME, sNextRouteName);
            bChanged = true;
        }

        return bChanged ? {
            activeObjectId: sNextActiveObjectId,
            currentRouteName: sNextRouteName,
            selectedId: sNextSelectedId
        } : null;
    }

    function RouteModeCoordinator(mDeps) {
        this._oRouter = mDeps.router;
        this._oStateModel = mDeps.stateModel;
        this._fnRouteMatched = this._onAnyRouteMatched.bind(this);
        this._fnHashChanged = this._onHashChanged.bind(this);
    }

    RouteModeCoordinator.prototype.start = function () {
        if (!this._oStateModel || !this._oRouter) {
            return;
        }
        this._oHashChanger = HashChanger.getInstance();
        this._oHashChanger.attachEvent("hashChanged", this._fnHashChanged);
        this._onHashChanged();
        debugLog("start");
    };

    RouteModeCoordinator.prototype.stop = function () {
        if (this._oHashChanger) {
            this._oHashChanger.detachEvent("hashChanged", this._fnHashChanged);
            this._oHashChanger = null;
        }
        debugLog("stop");
    };

    RouteModeCoordinator.prototype._onHashChanged = function () {
        var sHash = this._oHashChanger ? this._oHashChanger.getHash() : "";
        this._onAnyRouteMatched(RouteHashResolver.buildRouteEvent(RouteHashResolver.resolveRouteFromHash(sHash)));
    };

    RouteModeCoordinator.prototype._onAnyRouteMatched = function (oEvent) {
        var sRouteName = oEvent.getParameter("name");
        var mArgs = oEvent.getParameter("arguments") || {};
        var sNextLayout = RouteModeRules.resolveLayoutFromRoute(sRouteName, mArgs);
        var oRouteStateUpdate = syncRouteState(this._oStateModel, sRouteName, mArgs);

        if (oRouteStateUpdate) {
            debugLog("routeMatched", {
                expectedLayout: sNextLayout,
                route: sRouteName,
                selectedId: ModelStateRuntime.readOnModel(this._oStateModel, StatePaths.SELECTED_ID, null)
            });
        }
    };

    return RouteModeCoordinator;
});
