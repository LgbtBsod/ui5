sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellPaneConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ShellPaneContracts, ControllerModelRuntime, ModelStateRuntime) {
    "use strict";

    function writePaneLoaded(oController, sPaneKey, bLoaded) {
        var sPath = ShellPaneContracts.READINESS_PATHS[sPaneKey];
        var oStateModel = ControllerModelRuntime.state(oController);
        if (!sPath || !oStateModel) {
            return false;
        }
        return ModelStateRuntime.writeOnModel(oStateModel, sPath, !!bLoaded);
    }

    function ensurePaneForRoute(oController, sRouteName, NavigationContracts) {
        writePaneLoaded(oController, ShellPaneContracts.PANES.SEARCH, true);
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            writePaneLoaded(oController, ShellPaneContracts.PANES.ANALYTICS, true);
            return true;
        }
        if (NavigationContracts.isDetailRoute(sRouteName)) {
            writePaneLoaded(oController, ShellPaneContracts.PANES.DETAIL, true);
            return true;
        }
        return true;
    }

    return {
        ensurePaneForRoute: ensurePaneForRoute,
        writePaneLoaded: writePaneLoaded
    };
});
