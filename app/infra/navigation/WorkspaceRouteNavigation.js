sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "sap/ui/core/routing/HashChanger"
], function (CloneUtil, LayoutStateRuntime, ModelStateRuntime, ControllerModelRuntime, StatePaths, NavigationContracts, WorkflowContracts, HashChanger) {
    "use strict";

    function cloneArgs(oArgs) {
        return CloneUtil.clone(oArgs, {});
    }

    function readStateModel(oController) {
        return ControllerModelRuntime.state(oController);
    }

    function readSelectedId(oStateModel) {
        return String(
            (
                ModelStateRuntime.readOnModel(oStateModel, "/selectedId", "") ||
                ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "")
            ) || ""
        ).trim();
    }

    function buildFallbackIntent() {
        return {
            routeName: NavigationContracts.ROUTES.SEARCH,
            routeArgs: {}
        };
    }

    function buildCurrentIntent(oStateModel) {
        var sRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sSelectedId = readSelectedId(oStateModel);
        var sLayout = LayoutStateRuntime.readLayout(oStateModel, NavigationContracts.LAYOUTS.ONE_COLUMN);

        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return cloneArgs(ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", buildFallbackIntent()) || buildFallbackIntent());
        }
        if ((sRouteName === NavigationContracts.ROUTES.DETAIL_LAYOUT || sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) && sSelectedId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL_LAYOUT,
                routeArgs: {
                    id: sSelectedId,
                    layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
                }
            };
        }
        if ((sRouteName === NavigationContracts.ROUTES.DETAIL || sSelectedId) && sSelectedId) {
            return {
                routeName: NavigationContracts.ROUTES.DETAIL,
                routeArgs: { id: sSelectedId }
            };
        }
        return buildFallbackIntent();
    }

    function setAnalyticsReturnIntent(oController) {
        var oStateModel = readStateModel(oController);
        var oIntent = buildCurrentIntent(oStateModel);
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var sRootId = readSelectedId(oStateModel);
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
        var oStateModel = readStateModel(oController);
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sCurrentRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;

        if (sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            navigateToSearch(oController);
            return;
        }

        setAnalyticsReturnIntent(oController);
        ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.ANALYTICS);
        ModelStateRuntime.writeOnModel(oStateModel, "/layout", NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN);
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
        ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", oIntent.routeName || NavigationContracts.ROUTES.SEARCH);
        ModelStateRuntime.writeOnModel(
            oStateModel,
            "/layout",
            oIntent.routeName === NavigationContracts.ROUTES.ANALYTICS
                ? NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN
                : (oIntent.routeName === NavigationContracts.ROUTES.SEARCH
                    ? NavigationContracts.LAYOUTS.ONE_COLUMN
                    : LayoutStateRuntime.normalizeLayout((oIntent.routeArgs && oIntent.routeArgs.layout) || NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED))
        );
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        }
    }

    function navigateToSearch(oController) {
        var oStateModel = readStateModel(oController);
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
        ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.SEARCH);
        ModelStateRuntime.writeOnModel(oStateModel, "/layout", NavigationContracts.LAYOUTS.ONE_COLUMN);
        if (oHashChanger && typeof oHashChanger.replaceHash === "function") {
            oHashChanger.replaceHash("");
            return;
        }
        if (typeof window !== "undefined" && window.location) {
            window.location.hash = "";
            return;
        }
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(NavigationContracts.ROUTES.SEARCH, {}, false);
        }
    }

    function navigateToDetail(oController, sRootId, sLayout) {
        var oStateModel = readStateModel(oController);
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();
        var sResolvedLayout = LayoutStateRuntime.normalizeLayout(sLayout);

        if (!oRouter || typeof oRouter.navTo !== "function" || !sId) {
            return;
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/selectedId", sId);
        if (sResolvedLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.DETAIL_LAYOUT);
            ModelStateRuntime.writeOnModel(oStateModel, "/layout", NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN);
            oRouter.navTo(NavigationContracts.ROUTES.DETAIL_LAYOUT, { id: sId, layout: NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN }, false);
            return;
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", NavigationContracts.ROUTES.DETAIL);
        ModelStateRuntime.writeOnModel(oStateModel, "/layout", NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED);
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
