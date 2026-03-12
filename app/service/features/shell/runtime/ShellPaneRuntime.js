sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ShellPaneContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/JsRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStyleRuntime",
    "sap/ui/core/mvc/XMLView"
], function (ShellPaneContracts, JsRuntimeContracts, ControllerModelRuntime, ModelStateRuntime, ShellStyleRuntime, XMLView) {
    "use strict";

    var TYPE_FUNCTION = JsRuntimeContracts.TYPEOF.FUNCTION;

    function resolveHost(oController, sPaneKey) {
        var sHostId = ShellPaneContracts.HOST_IDS[sPaneKey];
        return sHostId && oController && oController.byId ? oController.byId(sHostId) : null;
    }

    function resolveViewId(oController, sPaneKey) {
        var sLocalId = ShellPaneContracts.VIEW_IDS[sPaneKey];
        var oView = oController && oController.getView ? oController.getView() : null;
        if (!sLocalId) {
            return "";
        }
        return oView && oView.createId ? oView.createId(sLocalId) : sLocalId;
    }

    function resolveNestedPaneView(oHost) {
        if (!oHost || typeof oHost.getItems !== TYPE_FUNCTION) {
            return null;
        }
        return oHost.getItems()[0] || null;
    }

    function writePaneLoaded(oController, sPaneKey, bLoaded) {
        var sPath = ShellPaneContracts.READINESS_PATHS[sPaneKey];
        var oStateModel = ControllerModelRuntime.state(oController);
        if (!sPath || !oStateModel) {
            return false;
        }
        return ModelStateRuntime.writeOnModel(oStateModel, sPath, !!bLoaded);
    }

    function ensurePaneView(oController, sPaneKey) {
        var oHost = resolveHost(oController, sPaneKey);
        var oPaneView;
        if (!oHost) {
            return null;
        }
        if (ShellPaneContracts.LAZY_PANES.indexOf(sPaneKey) === -1) {
            writePaneLoaded(oController, sPaneKey, true);
            return oHost;
        }
        oPaneView = resolveNestedPaneView(oHost);
        if (oPaneView) {
            writePaneLoaded(oController, sPaneKey, true);
            return oPaneView;
        }
        ShellStyleRuntime.ensurePaneStyles(sPaneKey);
        oPaneView = new XMLView(resolveViewId(oController, sPaneKey), {
            viewName: ShellPaneContracts.VIEW_NAMES[sPaneKey],
            width: "100%",
            height: "100%"
        });
        if (typeof oHost.addItem === TYPE_FUNCTION) {
            oHost.addItem(oPaneView);
        }
        writePaneLoaded(oController, sPaneKey, true);
        return oPaneView;
    }

    function ensurePaneForRoute(oController, sRouteName, NavigationContracts) {
        if (sRouteName === NavigationContracts.ROUTES.ANALYTICS) {
            return ensurePaneView(oController, ShellPaneContracts.PANES.ANALYTICS);
        }
        if (NavigationContracts.isDetailRoute(sRouteName)) {
            return ensurePaneView(oController, ShellPaneContracts.PANES.DETAIL);
        }
        return ensurePaneView(oController, ShellPaneContracts.PANES.SEARCH);
    }

    return {
        ensurePaneView: ensurePaneView,
        ensurePaneForRoute: ensurePaneForRoute,
        writePaneLoaded: writePaneLoaded
    };
});
