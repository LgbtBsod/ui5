sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "sap/ui/core/routing/HashChanger"
], function (CloneUtil, LayoutStateRuntime, ModelStateRuntime, ControllerModelRuntime, RootIdRuntime, StatePaths, NavigationContracts, WorkflowContracts, HashChanger) {
    "use strict";

    function cloneArgs(oArgs) {
        return CloneUtil.clone(oArgs, {});
    }

    function readStateModel(oController) {
        return ControllerModelRuntime.state(oController);
    }

    function buildFallbackIntent() {
        return {
            routeName: NavigationContracts.ROUTES.SEARCH,
            routeArgs: {}
        };
    }

    function buildCurrentIntent(oStateModel) {
        var sRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sActiveId = RootIdRuntime.resolveActiveFromStateModel(oStateModel);
        var sLayout = LayoutStateRuntime.readLayout(oStateModel, NavigationContracts.LAYOUTS.ONE_COLUMN);

        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return cloneArgs(ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", buildFallbackIntent()) || buildFallbackIntent());
        }
        if ((sRouteName === NavigationContracts.ROUTES.DETAIL_LAYOUT || sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) && sActiveId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL_LAYOUT,
                routeArgs: {
                    id: sActiveId,
                    layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
                }
            };
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL && sActiveId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL,
                routeArgs: { id: sActiveId }
            };
        }
        return buildFallbackIntent();
    }

    function setAnalyticsReturnIntent(oController) {
        var oStateModel = readStateModel(oController);
        var oIntent = buildCurrentIntent(oStateModel);
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var sRootId = RootIdRuntime.resolveFromStateModel(oStateModel);
        var bRestoreEdit = !!(sRootId && WorkflowContracts.isEditLocked(sMode, sLockState));

        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", {
            routeName: String(oIntent.routeName || NavigationContracts.ROUTES.SEARCH),
            routeArgs: cloneArgs(oIntent.routeArgs),
            rootId: sRootId,
            restoreEdit: bRestoreEdit
        });

        return oIntent;
    }

    function navigateToAnalytics(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oStateModel = readStateModel(oController);
        var sCurrentRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            navigateToSearch(oController);
            return;
        }

        setAnalyticsReturnIntent(oController);
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(NavigationContracts.ROUTES.ANALYTICS, {}, false);
        }
    }

    function navigateBackFromAnalytics(oController) {
        var oStateModel = readStateModel(oController);
        var oIntent = cloneArgs(ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", buildFallbackIntent()) || buildFallbackIntent());
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sTargetRootId;

        if (!oIntent.routeName) {
            oIntent = buildFallbackIntent();
        }
        sTargetRootId = String((oIntent && (oIntent.rootId || (oIntent.routeArgs && oIntent.routeArgs.id))) || "").trim();
        if (oIntent && oIntent.restoreEdit && sTargetRootId) {
            ModelStateRuntime.writeOnModel(oStateModel, "/analyticsReturnRestoreEdit", {
                rootId: sTargetRootId,
                requestedAt: new Date().toISOString()
            });
        } else {
            ModelStateRuntime.writeOnModel(oStateModel, "/analyticsReturnRestoreEdit", null);
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", null);
        if ((oIntent.routeName || NavigationContracts.ROUTES.SEARCH) === NavigationContracts.ROUTES.SEARCH) {
            navigateToSearch(oController);
            return;
        }
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        }
    }

    function navigateToSearch(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(NavigationContracts.ROUTES.SEARCH, {}, false);
            return;
        }
        if (oHashChanger && typeof oHashChanger.replaceHash === "function") {
            oHashChanger.replaceHash("");
            return;
        }
    }

    function navigateToDetail(oController, sRootId, sLayout) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();
        var sResolvedLayout = LayoutStateRuntime.normalizeLayout(sLayout);

        if (!oRouter || typeof oRouter.navTo !== "function" || !sId) {
            return;
        }
        if (sResolvedLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            oRouter.navTo(NavigationContracts.ROUTES.DETAIL_LAYOUT, { id: sId, layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN }, false);
            return;
        }
        oRouter.navTo(NavigationContracts.ROUTES.DETAIL, { id: sId }, false);
    }

    function buildDetailHash(oController, sRootId) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();

        if (!oRouter || typeof oRouter.getURL !== "function" || !sId) {
            return "";
        }
        return String(oRouter.getURL(NavigationContracts.ROUTES.DETAIL, { id: sId }) || "");
    }

    return {
        buildCurrentIntent: buildCurrentIntent,
        buildDetailHash: buildDetailHash,
        navigateBackFromAnalytics: navigateBackFromAnalytics,
        navigateToAnalytics: navigateToAnalytics,
        navigateToDetail: navigateToDetail,
        navigateToSearch: navigateToSearch,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent
    };
});
