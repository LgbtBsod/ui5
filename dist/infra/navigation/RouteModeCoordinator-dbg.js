sap.ui.define([
"PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeRules",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteSync",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (DebugLogger, RouteModeRules, RouteSync, LayoutStateRuntime, ModelStateRuntime, NavigationContracts) {
    "use strict";

    var LAYOUTS = NavigationContracts.LAYOUTS;

    function debugLog(sEvent, oPayload) {
        DebugLogger.info("RouteModeCoordinator", sEvent, oPayload || {});
    }

    function RouteModeCoordinator(mDeps) {
        this._oRouter = mDeps.router;
        this._oStateModel = mDeps.stateModel;
        this._oFcl = mDeps.fcl;
        this._fnRouteMatched = this._onAnyRouteMatched.bind(this);
    }

    RouteModeCoordinator.prototype._applyLayoutFromState = function () {
        var sLayout = LayoutStateRuntime.readLayout(this._oStateModel, LAYOUTS.ONE_COLUMN);
        if (this._oFcl && typeof this._oFcl.getLayout === "function" && typeof this._oFcl.setLayout === "function" && this._oFcl.getLayout() !== sLayout) {
            this._oFcl.setLayout(sLayout);
        }
    };

    RouteModeCoordinator.prototype.start = function () {
        if (!this._oStateModel || !this._oRouter) {
            return;
        }
        this._oRouter.attachRoutePatternMatched(this._fnRouteMatched);
        this._applyLayoutFromState();
        debugLog("start", {
            layout: LayoutStateRuntime.readLayout(this._oStateModel, LAYOUTS.ONE_COLUMN)
        });
    };

    RouteModeCoordinator.prototype.stop = function () {
        if (this._oRouter) {
            this._oRouter.detachRoutePatternMatched(this._fnRouteMatched);
        }
        debugLog("stop");
    };

    RouteModeCoordinator.prototype._onAnyRouteMatched = function (oEvent) {
        var sRouteName = oEvent.getParameter("name");
        var mArgs = oEvent.getParameter("arguments") || {};
        var sNextLayout = RouteModeRules.resolveLayoutFromRoute(sRouteName, mArgs);
        var oRouteSync = RouteSync.syncRouteState(this._oStateModel, sNextLayout, sRouteName, mArgs);
        if (oRouteSync) {
            this._applyLayoutFromState();
            debugLog("routeMatched", {
                route: sRouteName,
                layout: LayoutStateRuntime.readLayout(this._oStateModel, LAYOUTS.ONE_COLUMN),
                selectedId: ModelStateRuntime.readOnModel(this._oStateModel, "/selectedId", null)
            });
        }
    };

    return RouteModeCoordinator;
});
