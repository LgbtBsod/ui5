sap.ui.define([], function () {
    "use strict";

    function resolveView(oController) {
        return oController && oController.getView ? oController.getView() : null;
    }

    function resolveOwner(oController) {
        return oController && oController.getOwnerComponent ? oController.getOwnerComponent() : null;
    }

    function resolveNamedModel(oController, sName, bOwnerFallback) {
        var oView = resolveView(oController);
        var oOwner = bOwnerFallback === false ? null : resolveOwner(oController);
        return (oView && oView.getModel && oView.getModel(sName))
            || (oOwner && oOwner.getModel && oOwner.getModel(sName))
            || null;
    }

    function resolveDefaultModel(oController, bOwnerFallback) {
        return resolveNamedModel(oController, undefined, bOwnerFallback);
    }

    return {
        view: resolveView,
        owner: resolveOwner,
        named: resolveNamedModel,
        defaultModel: resolveDefaultModel,
        state: function (oController) {
            return resolveNamedModel(oController, "state", true);
        },
        layout: function (oController) {
            return resolveNamedModel(oController, "layout", true);
        },
        appView: function (oController) {
            return resolveNamedModel(oController, "appView", false);
        },
        viewState: function (oController) {
            return resolveNamedModel(oController, "view", false);
        },
        uiState: function (oController) {
            return resolveNamedModel(oController, "uiState", true);
        },
        selected: function (oController) {
            return resolveNamedModel(oController, "selected", true);
        },
        masterData: function (oController) {
            return resolveNamedModel(oController, "masterData", true);
        },
        env: function (oController) {
            return resolveNamedModel(oController, "env", true);
        }
    };
});
