sap.ui.define([
    "sap_ui5/util/DebugLogger",
    "sap_ui5/infra/navigation/RouteModeRules",
    "sap_ui5/infra/navigation/RouteSync"
], function (DebugLogger, RouteModeRules, RouteSync) {
    "use strict";

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
        var sLayout = String((this._oStateModel && this._oStateModel.getProperty && this._oStateModel.getProperty("/layout")) || "OneColumn");
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
            layout: this._oStateModel.getProperty("/layout") || "OneColumn"
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
                layout: this._oStateModel.getProperty("/layout"),
                selectedId: this._oStateModel.getProperty("/selectedId")
            });
        }
    };

    return RouteModeCoordinator;
});
