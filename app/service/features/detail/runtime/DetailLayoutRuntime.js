sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (LayoutStateRuntime, NavigationIntentService, ControllerModelRuntime, ModelStateRuntime, ModelPathContracts, ModelContracts, NavigationContracts) {
    "use strict";

    var SHELL_MODEL = ModelContracts.MODELS.SHELL;
    var DETAIL_MODEL = ModelContracts.MODELS.DETAIL;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;

    function resolveActiveDbKey(oController) {
        var oDetailModel = ControllerModelRuntime.detail(oController);
        var oStateModel = ControllerModelRuntime.state(oController);
        return String(
            ModelStateRuntime.readOnModel(oDetailModel, "/root/id", "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "")
            || ""
        ).trim();
    }

    return {
        applyLayoutState: function (oController, vLayout, mOptions) {
            var oRouter = oController.getRouter && oController.getRouter();
            var bSyncRoute = !mOptions || mOptions.syncRoute !== false;
            var sRootId;
            var sLayout;

            if (!ControllerModelRuntime.state(oController)) {
                return;
            }

            sRootId = resolveActiveDbKey(oController);
            sLayout = LayoutStateRuntime.normalizeLayout(vLayout);
            ModelStateRuntime.write(oController, SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, sLayout);
            if (!bSyncRoute || !oRouter) {
                return;
            }
            if (sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN) {
                NavigationIntentService.navigateToSearch(oController);
                return;
            }
            if (!sRootId) {
                return;
            }
            NavigationIntentService.navigateToDetail(oController, sRootId, sLayout);
        }
    };
});
