sap.ui.define([
    "checklist/app/util/CloneUtil"
], function (CloneUtil) {
    "use strict";

    function cloneArgs(oArgs) {
        return CloneUtil.clone(oArgs, {});
    }

    function normalizeLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        if (sLayout === "MidColumnFullScreen") {
            return "MidColumnFullScreen";
        }
        if (sLayout === "TwoColumnsMidExpanded" || sLayout === "TwoColumnsBeginExpanded") {
            return "TwoColumnsMidExpanded";
        }
        return "OneColumn";
    }

    function readStateModel(oController) {
        return oController && oController.getModel ? oController.getModel("state") : null;
    }

    function readSelectedId(oStateModel) {
        return String(
            (oStateModel && oStateModel.getProperty && (oStateModel.getProperty("/selectedId") || oStateModel.getProperty("/activeObjectId"))) || ""
        ).trim();
    }

    function buildFallbackIntent() {
        return {
            routeName: "search",
            routeArgs: {}
        };
    }

    function buildCurrentIntent(oStateModel) {
        var sRouteName = String((oStateModel && oStateModel.getProperty && oStateModel.getProperty("/currentRouteName")) || "search").trim() || "search";
        var sSelectedId = readSelectedId(oStateModel);
        var sLayout = normalizeLayout(oStateModel && oStateModel.getProperty && oStateModel.getProperty("/layout"));

        if (sRouteName === "analytics") {
            return cloneArgs((oStateModel && oStateModel.getProperty && oStateModel.getProperty("/analyticsNavReturn")) || buildFallbackIntent());
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

        if (oStateModel && typeof oStateModel.setProperty === "function") {
            oStateModel.setProperty("/analyticsNavReturn", {
                routeName: String(oIntent.routeName || "search"),
                routeArgs: cloneArgs(oIntent.routeArgs)
            });
        }

        return oIntent;
    }

    function navigateToAnalytics(oController) {
        var oRouter = oController && oController.getRouter && oController.getRouter();

        setAnalyticsReturnIntent(oController);
        if (oRouter && typeof oRouter.navTo === "function") {
            oRouter.navTo("analytics", {}, false);
        }
    }

    function navigateBackFromAnalytics(oController) {
        var oStateModel = readStateModel(oController);
        var oIntent = cloneArgs((oStateModel && oStateModel.getProperty && oStateModel.getProperty("/analyticsNavReturn")) || buildFallbackIntent());
        var oRouter = oController && oController.getRouter && oController.getRouter();

        if (!oIntent.routeName) {
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
        var sResolvedLayout = normalizeLayout(sLayout);

        if (!oRouter || typeof oRouter.navTo !== "function" || !sId) {
            return;
        }
        if (sResolvedLayout === "MidColumnFullScreen") {
            oRouter.navTo("detailLayout", { id: sId, layout: "MidColumnFullScreen" }, false);
            return;
        }
        oRouter.navTo("detail", { id: sId }, false);
    }

    function navigateToAccessDenied(oController, sRootId) {
        var oRouter = oController && oController.getRouter && oController.getRouter();
        var sId = String(sRootId || "").trim();

        if (oRouter && typeof oRouter.navTo === "function" && sId) {
            oRouter.navTo("accessDenied", { id: sId }, false);
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
