sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

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
        model: function (oController, sName, bOwnerFallback) {
            if (typeof sName === "undefined") {
                return resolveDefaultModel(oController, bOwnerFallback);
            }
            return resolveNamedModel(oController, sName, bOwnerFallback);
        },
        defaultModel: resolveDefaultModel,
        state: function (oController) {
            return resolveNamedModel(oController, MODELS.STATE, true);
        },
        detail: function (oController) {
            return resolveNamedModel(oController, MODELS.DETAIL, true);
        },
        shell: function (oController) {
            return resolveNamedModel(oController, MODELS.SHELL, true);
        },
        viewState: function (oController) {
            return resolveNamedModel(oController, MODELS.VIEW, false);
        },
        masterData: function (oController) {
            return resolveNamedModel(oController, MODELS.MASTER_DATA, true);
        }
    };
});
