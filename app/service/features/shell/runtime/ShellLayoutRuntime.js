sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (LayoutStateRuntime, RootIdRuntime, ModelStateRuntime, ThemeDomRuntime, NavigationContracts, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;

    function resolveRouteNameFromHash() {
        var sHash;
        if (typeof window === "undefined" || !window.location) {
            return "";
        }
        sHash = String(window.location.hash || "").trim();
        if (/^#\/analytics(?:$|\?)/.test(sHash)) {
            return NavigationContracts.ROUTES.ANALYTICS;
        }
        if (/^#\/checklist\/[^/]+\/MidColumnFullScreen(?:$|\?)/.test(sHash)) {
            return NavigationContracts.ROUTES.DETAIL_LAYOUT;
        }
        if (/^#\/checklist\/[^/]+(?:$|\?)/.test(sHash)) {
            return NavigationContracts.ROUTES.DETAIL;
        }
        return NavigationContracts.ROUTES.SEARCH;
    }

    function syncMidColumnPage(oController, sRouteName) {
        var oLayout = oController.byId && oController.byId("mainFcl");
        var oTargetPage = oController.byId && oController.byId(NavigationContracts.resolveMidColumnPageId(sRouteName));
        var oCurrentPage;

        if (!oLayout || !oTargetPage || typeof oLayout.toMidColumnPage !== "function") {
            return;
        }
        oCurrentPage = oLayout.getCurrentMidColumnPage && oLayout.getCurrentMidColumnPage();
        if (oCurrentPage && oCurrentPage.getId && oCurrentPage.getId() === oTargetPage.getId()) {
            return;
        }
        oLayout.toMidColumnPage(oTargetPage);
    }

    function syncLayoutState(oController, oStateModel) {
        var sLayoutRaw = ModelStateRuntime.read(oController, STATE_MODEL, "/layout", NavigationContracts.LAYOUTS.ONE_COLUMN);
        var sLayout = LayoutStateRuntime.normalizeLayout(sLayoutRaw);
        var sRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sHashRouteName = resolveRouteNameFromHash();
        var sSelectedId = RootIdRuntime.resolveFromStateModel(oStateModel);
        var bSingle = sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN;
        var bDetailOnly = sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        var oRoot = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oClassHost = (oRoot && oRoot.querySelector && oRoot.querySelector(".chkSkin")) || oRoot;
        var oLayout = oController.byId && oController.byId("mainFcl");

        if (sHashRouteName && sHashRouteName !== sRouteName) {
            sRouteName = sHashRouteName;
            if (oStateModel && oStateModel.setProperty) {
                ModelStateRuntime.writeOnModel(oStateModel, "/currentRouteName", sRouteName);
            }
        }
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS && sLayout !== NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            sLayout = NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
            bSingle = false;
            bDetailOnly = true;
        }
        if (!sSelectedId && sLayout !== NavigationContracts.LAYOUTS.ONE_COLUMN && sRouteName !== NavigationContracts.ROUTES.ANALYTICS) {
            sLayout = NavigationContracts.LAYOUTS.ONE_COLUMN;
            bSingle = true;
            bDetailOnly = false;
        }
        if (oClassHost && oClassHost.classList) {
            ThemeDomRuntime.toggleClass([oClassHost], "appLayoutSingle", bSingle);
            ThemeDomRuntime.toggleClass([oClassHost], "appLayoutSplit", !bSingle && !bDetailOnly);
            ThemeDomRuntime.toggleClass([oClassHost], "appLayoutDetailOnly", bDetailOnly);
        }
        if (oLayout && typeof oLayout.getLayout === "function" && typeof oLayout.setLayout === "function" && oLayout.getLayout() !== sLayout) {
            oLayout.setLayout(sLayout);
        }
        syncMidColumnPage(oController, sRouteName);
        if (oStateModel && oStateModel.setProperty && sLayoutRaw !== sLayout) {
            ModelStateRuntime.writeOnModel(oStateModel, "/layout", sLayout);
        }
    }

    return {
        syncLayoutState: syncLayoutState
    };
});
