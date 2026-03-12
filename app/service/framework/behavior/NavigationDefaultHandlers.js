sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (WorkspaceRouteNavigation, BehaviorRegistry, ModelStateRuntime, HashChanger, NavigationContracts) {
    "use strict";

    var NAVIGATION_SCOPE = "navigation";
    var bDefaultsRegistered = false;

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function cloneRouteArgs(oRouteArgs) {
        return oRouteArgs ? Object.assign({}, oRouteArgs) : {};
    }

    function buildIntentHash(mContext, oIntent) {
        var oComponent = mContext && mContext.component;
        var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
        var sRouteName = normalizeRouteName(oIntent && oIntent.routeName);
        var oRouteArgs = cloneRouteArgs(oIntent && oIntent.routeArgs);
        var sUrl;

        if (!sRouteName) {
            return "";
        }
        if (oRouter && typeof oRouter.getURL === "function") {
            sUrl = String(oRouter.getURL(sRouteName, oRouteArgs) || "");
            if (!sUrl) {
                return "#";
            }
            return sUrl.charAt(0) === "/" ? "#" + sUrl : "#/" + sUrl;
        }
        if (sRouteName === NavigationContracts.ROUTES.SEARCH) {
            return "#";
        }
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return "#/" + NavigationContracts.ROUTES.ANALYTICS;
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL_LAYOUT && oRouteArgs.id && oRouteArgs.layout) {
            return "#/checklist/" + encodeURIComponent(String(oRouteArgs.id)) + "/" + encodeURIComponent(String(oRouteArgs.layout));
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL && oRouteArgs.id) {
            return "#/checklist/" + encodeURIComponent(String(oRouteArgs.id));
        }
        return "";
    }

    function queuePendingIntent(mContext) {
        var oCurrentIntent = WorkspaceRouteNavigation.buildCurrentIntent(mContext.stateModel) || {};
        ModelStateRuntime.writeOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, {
            routeName: mContext.routeEvent && mContext.routeEvent.getParameter && mContext.routeEvent.getParameter("name"),
            routeArgs: (mContext.routeEvent && mContext.routeEvent.getParameter && mContext.routeEvent.getParameter("arguments")) || {},
            currentIntent: {
                routeName: normalizeRouteName(oCurrentIntent.routeName),
                routeArgs: cloneRouteArgs(oCurrentIntent.routeArgs)
            },
            currentHash: buildIntentHash(mContext, oCurrentIntent),
            queuedAt: new Date().toISOString()
        });
    }

    function clearPendingIntent(mContext) {
        ModelStateRuntime.writeOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
    }

    function revertPendingIntent(mContext) {
        var oIntent = ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
        var sTargetHash = String((oIntent && oIntent.currentHash) || "").trim();
        var oHashChanger;
        if (!sTargetHash) {
            return false;
        }
        oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
        if (oHashChanger && typeof oHashChanger.replaceHash === "function") {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            oHashChanger.replaceHash(sTargetHash.replace(/^#\/?/, ""));
            return true;
        }
        if (typeof window !== "undefined" && window.location) {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            window.location.hash = sTargetHash;
            return true;
        }
        return false;
    }

    function resumePendingIntent(mContext) {
        var oIntent = ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
        if (!oIntent || !oIntent.routeName) {
            return false;
        }
        clearPendingIntent(mContext);
        ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
        mContext.component.getRouter().navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    function restorePendingIntent(mContext) {
        var oIntent = ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
        var oCurrentIntent;
        var oComponent = mContext && mContext.component;
        var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
        var sCurrentHash;
        var sTargetHash;
        if (!oIntent) {
            return false;
        }
        oCurrentIntent = oIntent.currentIntent || {};
        clearPendingIntent(mContext);
        if (oRouter && typeof oRouter.navTo === "function" && normalizeRouteName(oCurrentIntent.routeName)) {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            oRouter.navTo(oCurrentIntent.routeName, cloneRouteArgs(oCurrentIntent.routeArgs), false);
            return true;
        }
        sTargetHash = String(oIntent.currentHash || "").trim();
        if (!sTargetHash || typeof window === "undefined" || !window.location) {
            return false;
        }
        sCurrentHash = String(window.location.hash || "");
        if (sCurrentHash === sTargetHash) {
            return true;
        }
        ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
        window.location.hash = sTargetHash;
        return true;
    }

    var mHandlers = {
        buildCurrentIntent: function (mContext) {
            return WorkspaceRouteNavigation.buildCurrentIntent(mContext.stateModel);
        },
        setAnalyticsReturnIntent: function (mContext) {
            return WorkspaceRouteNavigation.setAnalyticsReturnIntent(mContext.controller);
        },
        navigateToSearch: function (mContext) {
            return WorkspaceRouteNavigation.navigateToSearch(mContext.controller);
        },
        navigateToDetail: function (mContext) {
            return WorkspaceRouteNavigation.navigateToDetail(mContext.controller, mContext.rootId, mContext.layout);
        },
        buildDetailHash: function (mContext) {
            return WorkspaceRouteNavigation.buildDetailHash(mContext.controller, mContext.rootId);
        },
        navigateToAnalytics: function (mContext) {
            return WorkspaceRouteNavigation.navigateToAnalytics(mContext.controller);
        },
        navigateBackFromAnalytics: function (mContext) {
            return WorkspaceRouteNavigation.navigateBackFromAnalytics(mContext.controller);
        },
        queuePendingIntent: queuePendingIntent,
        clearPendingIntent: clearPendingIntent,
        revertPendingIntent: revertPendingIntent,
        resumePendingIntent: resumePendingIntent,
        restorePendingIntent: restorePendingIntent
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(NAVIGATION_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
