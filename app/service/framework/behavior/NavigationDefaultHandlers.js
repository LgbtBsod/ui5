sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (WorkspaceRouteNavigation, BehaviorRegistry, ModelStateRuntime, HashChanger, NavigationContracts, JsRuntime) {
    "use strict";

    var NAVIGATION_SCOPE = "navigation";
    var HASH_PREFIX = "#";
    var HASH_ROUTE_PREFIX = "#/";
    var bDefaultsRegistered = false;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var HASH_CHANGER = JsRuntime.HASH_CHANGER;
    var DEFAULT_INTENT_OWNER = "unknown";
    var OWNER_NAVIGATION_GUARD = "navigationGuard";
    var RESUME_MODE_AFTER_GUARDED_SAVE = "afterGuardedSave";

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function cloneRouteArgs(oRouteArgs) {
        return oRouteArgs ? Object.assign({}, oRouteArgs) : {};
    }

    function normalizePendingIntent(oIntent, mDefaults) {
        var oResolved = Object.assign({
            routeName: "",
            routeArgs: {},
            owner: DEFAULT_INTENT_OWNER,
            resumeMode: "",
            createdAt: ""
        }, oIntent || {}, mDefaults || {});

        oResolved.routeName = normalizeRouteName(oResolved.routeName);
        oResolved.routeArgs = cloneRouteArgs(oResolved.routeArgs);
        oResolved.owner = String(oResolved.owner || DEFAULT_INTENT_OWNER).trim() || DEFAULT_INTENT_OWNER;
        oResolved.resumeMode = String(oResolved.resumeMode || "").trim();
        oResolved.createdAt = String(oResolved.createdAt || "").trim();
        return oResolved;
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
        var oCurrentIntent = WorkspaceRouteNavigation.buildCurrentIntent(mContext.stateModel) || {};
        ModelStateRuntime.writeOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, normalizePendingIntent({
            routeName: mContext.routeEvent && typeof mContext.routeEvent.getParameter === TYPE_FUNCTION && mContext.routeEvent.getParameter("name"),
            routeArgs: (mContext.routeEvent && typeof mContext.routeEvent.getParameter === TYPE_FUNCTION && mContext.routeEvent.getParameter("arguments")) || {},
            currentIntent: {
                routeName: normalizeRouteName(oCurrentIntent.routeName),
                routeArgs: cloneRouteArgs(oCurrentIntent.routeArgs)
            },
            currentHash: buildIntentHash(mContext, oCurrentIntent),
            owner: mContext.owner || OWNER_NAVIGATION_GUARD,
            resumeMode: mContext.resumeMode || RESUME_MODE_AFTER_GUARDED_SAVE,
            queuedAt: new Date().toISOString(),
            createdAt: new Date().toISOString()
        }));
    }

    function clearPendingIntent(mContext) {
        ModelStateRuntime.writeOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null);
    }

    function revertPendingIntent(mContext) {
        var oIntent = normalizePendingIntent(ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null));
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
        var oIntent = normalizePendingIntent(ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null));
        if (!oIntent || !oIntent.routeName) {
            return false;
        }
        clearPendingIntent(mContext);
        ModelStateRuntime.writeOnModel(mContext.stateModel, "/navGuardBypass", true);
        mContext.component.getRouter()[METHODS.NAV_TO](oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    function restorePendingIntent(mContext) {
        var oIntent = normalizePendingIntent(ModelStateRuntime.readOnModel(mContext.stateModel, mContext.statePaths.PENDING_NAVIGATION_INTENT, null));
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
        ensureRegistered: ensureRegistered,
        normalizePendingIntent: normalizePendingIntent,
        OWNER_NAVIGATION_GUARD: OWNER_NAVIGATION_GUARD,
        RESUME_MODE_AFTER_GUARDED_SAVE: RESUME_MODE_AFTER_GUARDED_SAVE
    };
});
