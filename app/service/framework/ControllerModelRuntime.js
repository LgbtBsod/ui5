sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ModelContracts, ModelStateRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

    function resolveView(oController) {
        return oController && oController.getView ? oController.getView() : null;
    }

    function resolveOwner(oController) {
        return oController && oController.getOwnerComponent ? oController.getOwnerComponent() : null;
    }

    function resolveDirectModelHost(oController) {
        if (!oController || typeof oController.getModel !== "function") {
            return null;
        }
        if (typeof oController.getView === "function") {
            return null;
        }
        return oController;
    }

    function resolveNamedModel(oController, sName, bOwnerFallback) {
        var oView = resolveView(oController);
        var oDirectHost = resolveDirectModelHost(oController);
        var oOwner = bOwnerFallback === false ? null : resolveOwner(oController);
        return (oView && oView.getModel && oView.getModel(sName))
            || (oDirectHost && oDirectHost.getModel && oDirectHost.getModel(sName))
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
        named: resolveNamedModel,
        defaultModel: resolveDefaultModel,
        read: function (oController, sName, sPath, vFallback) {
            return ModelStateRuntime.readOnModel(resolveNamedModel(oController, sName, true), sPath, vFallback);
        },
        write: function (oController, sName, sPath, vValue) {
            return ModelStateRuntime.writeOnModel(resolveNamedModel(oController, sName, true), sPath, vValue);
        },
        setMany: function (oController, sName, mValues) {
            return ModelStateRuntime.setManyOnModel(resolveNamedModel(oController, sName, true), mValues);
        },
        state: function (oController) {
            return resolveNamedModel(oController, MODELS.STATE, true);
        },
        shell: function (oController) {
            return resolveNamedModel(oController, MODELS.SHELL, true);
        },
        viewState: function (oController) {
            return resolveNamedModel(oController, MODELS.VIEW, false);
        },
        selected: function (oController) {
            return resolveNamedModel(oController, MODELS.SELECTED, true);
        },
        snapshot: function (oController) {
            return resolveNamedModel(oController, MODELS.SNAPSHOT, true);
        },
        masterData: function (oController) {
            return resolveNamedModel(oController, MODELS.MASTER_DATA, true);
        },
        locationTree: function (oController) {
            return resolveNamedModel(oController, MODELS.LOCATION_TREE, true);
        },
        env: function (oController) {
            return resolveNamedModel(oController, MODELS.ENV, true);
        }
    };
});
