sap.ui.define([
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/util/CloneUtil"
], function (ModelStateRuntime, CloneUtil) {
    "use strict";

    function getModel(oController, sModelName) {
        if (!oController || !oController.getModel) { return null; }
        return oController.getModel(sModelName);
    }

    function busy(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var sPath = oEffect.path || (oEffect.scope ? "/busy/" + oEffect.scope : "/busy");
        ModelStateRuntime.writeOnModel(oModel, sPath, !!oEffect.value);
    }

    function patch(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        ModelStateRuntime.writeOnModel(oModel, oEffect.path, CloneUtil.clone(oEffect.value));
    }

    function merge(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var oCurrent;
        if (!oModel || !oModel.getProperty || !oModel.setProperty) { return; }
        oCurrent = ModelStateRuntime.readOnModel(oModel, oEffect.path, {}) || {};
        ModelStateRuntime.writeOnModel(
            oModel,
            oEffect.path,
            Object.assign({}, CloneUtil.clone(oCurrent), CloneUtil.clone(oEffect.partialObject || {}))
        );
    }

    return { busy: busy, patch: patch, merge: merge };
});
