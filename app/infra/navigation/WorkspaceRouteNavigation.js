sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime"
], function (CloneUtil, LayoutStateRuntime, ModelStateRuntime, ControllerModelRuntime) {
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
        if (sRouteName === "accessDenied" && sSelectedId) {
            return {
                routeName: "accessDenied",
                routeArgs: { id: sSelectedId }
            };
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
        if (String(oIntent.routeName || "") === "accessDenied") {
            oIntent = buildFallbackIntent();
        }

        ModelStateRuntime.writeOnModel(oStateModel, "/analyticsNavReturn", {
            routeName: String(oIntent.routeName || "search"),
            routeArgs: cloneArgs(oIntent.routeArgs)
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

        if (!oIntent.routeName) {
            oIntent = buildFallbackIntent();
        }
        if (String(oIntent.routeName || "") === "accessDenied") {
            oIntent = buildFallbackIntent();
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

    function navigateToAccessDenied(oController, sRootId, mOptions) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();
        var bFullScreen = !!(mOptions && mOptions.fullScreen);

        if (oRouter && typeof oRouter.navTo === "function" && sId) {
            if (bFullScreen) {
                oRouter.navTo("accessDenied", { id: sId }, false);
                return;
            }
            oRouter.navTo("detail", { id: sId }, false);
        }
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
        navigateToAccessDenied: navigateToAccessDenied,
        navigateToAnalytics: navigateToAnalytics,
        navigateToDetail: navigateToDetail,
        navigateToSearch: navigateToSearch,
        setAnalyticsReturnIntent: setAnalyticsReturnIntent
    };
});
