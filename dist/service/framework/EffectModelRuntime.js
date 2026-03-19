sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StyleAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/ui/StyleTokens",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (Ui5StyleAdapter, StyleTokens, ModelStateRuntime, CloneUtil, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

    function getModel(oController, sModelName) {
        if (!oController || !oController.getModel) {
            return null;
        }
        return oController.getModel(sModelName);
    }

    function busyModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var sPath = oEffect.path || (oEffect.scope ? "/busy/" + oEffect.scope : "/busy");
        ModelStateRuntime.writeOnModel(oModel, sPath, !!oEffect.value);
    }

    function patchModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        ModelStateRuntime.writeOnModel(oModel, oEffect.path, CloneUtil.clone(oEffect.value));
    }

    function mergeModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var oCurrent;
        if (!oModel || !oModel.getProperty || !oModel.setProperty) {
            return;
        }
        oCurrent = ModelStateRuntime.readOnModel(oModel, oEffect.path, {}) || {};
        ModelStateRuntime.writeOnModel(
            oModel,
            oEffect.path,
            Object.assign({}, CloneUtil.clone(oCurrent), CloneUtil.clone(oEffect.partialObject || {}))
        );
    }

    function inlineValidation(oController, oEffect) {
        return patchModel(oController, {
            modelName: oEffect.modelName || MODELS.STATE,
            path: oEffect.path || "/ui/feedback/inlineErrors",
            value: oEffect.value || {}
        });
    }

    function styleTokenEnable(oController, oEffect) {
        return Ui5StyleAdapter.enable(oController, StyleTokens.resolveClassName(oEffect.token), oEffect.target || MODELS.VIEW);
    }

    function styleTokenDisable(oController, oEffect) {
        return Ui5StyleAdapter.disable(oController, StyleTokens.resolveClassName(oEffect.token), oEffect.target || MODELS.VIEW);
    }

    return {
        busyModel: busyModel,
        patchModel: patchModel,
        mergeModel: mergeModel,
        inlineValidation: inlineValidation,
        styleTokenEnable: styleTokenEnable,
        styleTokenDisable: styleTokenDisable
    };
});
