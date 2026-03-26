sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (LayoutStateRuntime, ModelStateRuntime, ThemeDomRuntime, NavigationContracts, ModelContracts, ModelPathContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var STATE_MODEL = MODELS.STATE;
    var SHELL_MODEL = MODELS.SHELL;

    function resolveDesiredLayout(oController, oStateModel) {
        var oShellModel = ModelStateRuntime.model(oController, SHELL_MODEL);
        var sCurrentLayout = LayoutStateRuntime.readLayout(oShellModel, NavigationContracts.LAYOUTS.ONE_COLUMN);
        var sRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var sSelectedId = String(
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
            || ""
        ).trim();

        if (sRouteName === NavigationContracts.ROUTES.SEARCH) {
            return NavigationContracts.LAYOUTS.ONE_COLUMN;
        }
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        }
        if (sRouteName === NavigationContracts.ROUTES.DETAIL) {
            return sSelectedId ? NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED : NavigationContracts.LAYOUTS.ONE_COLUMN;
        }
        if (!sSelectedId) {
            return NavigationContracts.LAYOUTS.ONE_COLUMN;
        }
        return LayoutStateRuntime.normalizeLayout(sCurrentLayout);
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
        var oShellModel = ModelStateRuntime.model(oController, SHELL_MODEL);
        var sLayout = resolveDesiredLayout(oController, oStateModel);
        var sRouteName = String(ModelStateRuntime.read(oController, STATE_MODEL, ModelPathContracts.CURRENT_ROUTE_NAME, NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
        var bSingle = sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN;
        var bDetailOnly = sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        var oRoot = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oClassHost = (oRoot && oRoot.querySelector && oRoot.querySelector(".chkSkin")) || oRoot;
        var oLayout = oController.byId && oController.byId("mainFcl");

        if (oShellModel && ModelStateRuntime.readOnModel(oShellModel, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN) !== sLayout) {
            ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_LAYOUT, sLayout);
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
    }

    return {
        resolveDesiredLayout: resolveDesiredLayout,
        syncLayoutState: syncLayoutState
    };
});
