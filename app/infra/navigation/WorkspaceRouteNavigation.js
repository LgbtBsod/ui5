sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (CloneUtil, LayoutStateRuntime, ModelStateRuntime, ControllerModelRuntime, StatePaths) {
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
            routeName: "search",
            routeArgs: {}
        };
    }

    function buildCurrentIntent(oStateModel) {
        var sRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", "search") || "search").trim() || "search";
        var sSelectedId = readSelectedId(oStateModel);
        var sLayout = LayoutStateRuntime.readLayout(oStateModel, "OneColumn");

        if (sRouteName === "analytics") {
            return cloneArgs(ModelStateRuntime.readOnModel(oStateModel, "/analyticsNavReturn", buildFallbackIntent()) || buildFallbackIntent());
        }
        if ((sRouteName === "detailLayout" || sLayout === "MidColumnFullScreen") && sSelectedId) {
            return {
                routeName: "detailLayout",
                routeArgs: {
                    id: sSelectedId,
                    layout: "MidColumnFullScreen"
                }
            };
        }
        if ((sRouteName === "detail" || sSelectedId) && sSelectedId) {
            return {
                routeName: "detail",
                routeArgs: { id: sSelectedId }
            };
        }
        return buildFallbackIntent();
    }

    function setAnalyticsReturnIntent(oController) {
        var oStateModel = readStateModel(oController);
        var oIntent = buildCurrentIntent(oStateModel);
        var sMode = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ") || "READ").trim().toUpperCase();
        var sLockState = String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "READ_ONLY") || "READ_ONLY").trim().toUpperCase();
        var sRootId = readSelectedId(oStateModel);
        var bRestoreEdit = !!(sRootId && sMode === "EDIT" && sLockState === "EDIT_LOCKED");

        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", {
            routeName: String(oIntent.routeName || "search"),
            routeArgs: cloneArgs(oIntent.routeArgs),
            rootId: sRootId,
            restoreEdit: bRestoreEdit
        });

        return oIntent;
    }

    function navigateToAnalytics(oController) {
        var oStateModel = readStateModel(oController);
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sCurrentRouteName = String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", "search") || "search").trim() || "search";

        if (sCurrentRouteName === "analytics") {
            navigateToSearch(oController);
            return;
        }

        setAnalyticsReturnIntent(oController);
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo("analytics", {}, false);
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
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        }
    }

    function navigateToSearch(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo("search", {}, false);
        }
    }

    function navigateToDetail(oController, sRootId, sLayout) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();
        var sResolvedLayout = LayoutStateRuntime.normalizeLayout(sLayout);

        if (!oRouter || typeof oRouter.navTo !== "function" || !sId) {
            return;
        }
        if (sResolvedLayout === "MidColumnFullScreen") {
            oRouter.navTo("detailLayout", { id: sId, layout: "MidColumnFullScreen" }, false);
            return;
        }
        oRouter.navTo("detail", { id: sId }, false);
    }

    function buildDetailHash(oController, sRootId) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();

        if (!oRouter || typeof oRouter.getURL !== "function" || !sId) {
            return "";
        }
        return String(oRouter.getURL("detail", { id: sId }) || "");
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
