sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (LayoutStateRuntime, WorkspaceRouteNavigation, ModelStateRuntime, StatePaths, ModelContracts, NavigationContracts) {
    "use strict";

    var SHELL_MODEL = ModelContracts.MODELS.SHELL;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;

    function getModel(oController, sName) {
        return oController && oController.getModel ? oController.getModel(sName) : null;
    }

    function resolveActiveDbKey(oController) {
        var oDetailModel = getModel(oController, ModelContracts.MODELS.DETAIL);
        var oStateModel = getModel(oController, ModelContracts.MODELS.STATE);
        return String(
            ModelStateRuntime.readOnModel(oDetailModel, "/root/id", "")
            || ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, StatePaths.SELECTED_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, StatePaths.POST_OPEN_HYDRATED_ROOT_ID, "")
            || ""
        ).trim();
    }

    return {
        applyLayoutState: function (oController, vLayout, mOptions) {
            var oRouter = oController.getRouter && oController.getRouter();
            var bSyncRoute = !mOptions || mOptions.syncRoute !== false;
            var sDbKey;
            var sLayout;

            if (!getModel(oController, ModelContracts.MODELS.STATE)) {
                return;
            }

            sDbKey = resolveActiveDbKey(oController);
            sLayout = LayoutStateRuntime.normalizeLayout(vLayout);
            ModelStateRuntime.write(oController, SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, sLayout);
            if (!bSyncRoute || !oRouter) {
                return;
            }
            if (sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN) {
                WorkspaceRouteNavigation.navigateToSearch(oController);
                return;
            }
            if (!sDbKey) {
                return;
            }
            WorkspaceRouteNavigation.navigateToDetail(oController, sDbKey, sLayout);
        }
    };
});
