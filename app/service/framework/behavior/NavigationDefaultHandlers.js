sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/NavigationBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (NavigationBehaviorHelpers, BehaviorRegistry, ModelStateRuntime, HashChanger, NavigationContracts, JsRuntime) {
    "use strict";

    var NAVIGATION_SCOPE = "navigation";
    var HASH_PREFIX = "#";
    var HASH_ROUTE_PREFIX = "#/";
    var bDefaultsRegistered = false;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var HASH_CHANGER = JsRuntime.HASH_CHANGER;

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function cloneRouteArgs(oRouteArgs) {
        return oRouteArgs ? Object.assign({}, oRouteArgs) : {};
    }

    function buildIntentHash(mContext, oIntent) {
        var oComponent = mContext && mContext.component;
        var oRouter = oComponent && typeof oComponent.getRouter === TYPE_FUNCTION ? oComponent.getRouter() : null;
        var sRouteName = normalizeRouteName(oIntent && oIntent.routeName);
        var oRouteArgs = cloneRouteArgs(oIntent && oIntent.routeArgs);
        var sUrl;

        if (!sRouteName) {
            return "";
        }
        if (oRouter && typeof oRouter[METHODS.GET_URL] === TYPE_FUNCTION) {
            sUrl = String(oRouter[METHODS.GET_URL](sRouteName, oRouteArgs) || "");
            if (!sUrl) {
                return HASH_PREFIX;
            }
            return sUrl.charAt(0) === "/" ? HASH_PREFIX + sUrl : HASH_ROUTE_PREFIX + sUrl;
        }
        if (sRouteName === NavigationContracts.ROUTES.SEARCH) {
            return HASH_PREFIX;
        }
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return HASH_ROUTE_PREFIX + NavigationContracts.ROUTES.ANALYTICS;
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL && oRouteArgs.id) {
            if (oRouteArgs.layout) {
                return "#/checklist/" + encodeURIComponent(String(oRouteArgs.id)) + "/" + encodeURIComponent(String(oRouteArgs.layout));
            }
            return "#/checklist/" + encodeURIComponent(String(oRouteArgs.id));
        }
        return "";
    }

    function queuePendingIntent(mContext) {
        var oCurrentIntent = NavigationBehaviorHelpers.buildCurrentIntent(mContext.stateModel) || {};
        ModelStateRuntime.writeOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, {
            routeName: mContext.routeEvent && typeof mContext.routeEvent.getParameter === TYPE_FUNCTION && mContext.routeEvent.getParameter("name"),
            routeArgs: (mContext.routeEvent && typeof mContext.routeEvent.getParameter === TYPE_FUNCTION && mContext.routeEvent.getParameter("arguments")) || {},
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
        oHashChanger = HashChanger && typeof HashChanger.getInstance === TYPE_FUNCTION ? HashChanger.getInstance() : null;
        if (oHashChanger && typeof oHashChanger[HASH_CHANGER.REPLACE_HASH] === TYPE_FUNCTION) {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            oHashChanger[HASH_CHANGER.REPLACE_HASH](sTargetHash.replace(/^#\/?/, ""));
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
        mContext.component.getRouter()[METHODS.NAV_TO](oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    function restorePendingIntent(mContext) {
        var oIntent = ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
        var oCurrentIntent;
        var oComponent = mContext && mContext.component;
        var oRouter = oComponent && typeof oComponent.getRouter === TYPE_FUNCTION ? oComponent.getRouter() : null;
        var oHashChanger = HashChanger && typeof HashChanger.getInstance === TYPE_FUNCTION ? HashChanger.getInstance() : null;
        var sTargetHash;
        if (!oIntent) {
            return false;
        }
        oCurrentIntent = oIntent.currentIntent || {};
        clearPendingIntent(mContext);
        if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPE_FUNCTION && normalizeRouteName(oCurrentIntent.routeName)) {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            oRouter[METHODS.NAV_TO](oCurrentIntent.routeName, cloneRouteArgs(oCurrentIntent.routeArgs), false);
            return true;
        }
        sTargetHash = String(oIntent.currentHash || "").trim();
        if (!sTargetHash) {
            return false;
        }
        if (oHashChanger && typeof oHashChanger[HASH_CHANGER.REPLACE_HASH] === TYPE_FUNCTION) {
            ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
            oHashChanger[HASH_CHANGER.REPLACE_HASH](sTargetHash.replace(/^#\/?/, ""));
            return true;
        }
        return false;
    }

    var mHandlers = {
        buildCurrentIntent: function (mContext) {
            return NavigationBehaviorHelpers.buildCurrentIntent(mContext.stateModel);
        },
        setAnalyticsReturnIntent: function (mContext) {
            return NavigationBehaviorHelpers.setAnalyticsReturnIntent(mContext.controller);
        },
        navigateToSearch: function (mContext) {
            return NavigationBehaviorHelpers.navigateToSearch(mContext.controller);
        },
        navigateToDetail: function (mContext) {
            return NavigationBehaviorHelpers.navigateToDetail(mContext.controller, mContext.rootId, mContext.layout);
        },
        buildDetailHash: function (mContext) {
            return NavigationBehaviorHelpers.buildDetailHash(mContext.controller, mContext.rootId);
        },
        navigateToAnalytics: function (mContext) {
            return NavigationBehaviorHelpers.navigateToAnalytics(mContext.controller);
        },
        navigateBackFromAnalytics: function (mContext) {
            return NavigationBehaviorHelpers.navigateBackFromAnalytics(mContext.controller);
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
