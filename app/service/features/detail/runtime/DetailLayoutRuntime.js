sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (LayoutStateRuntime, NavigationIntentService, RootIdRuntime, ControllerModelRuntime, ModelStateRuntime, ModelContracts, NavigationContracts) {
    "use strict";

    var SHELL_MODEL = ModelContracts.MODELS.SHELL;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;

    return {
        applyLayoutState: function (oController, vLayout, mOptions) {
            var oRouter = oController.getRouter && oController.getRouter();
            var bSyncRoute = !mOptions || mOptions.syncRoute !== false;
            var sRootId;
            var sLayout;

            if (!ControllerModelRuntime.state(oController)) {
                return;
            }

            sRootId = RootIdRuntime.resolveFromController(oController);
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
