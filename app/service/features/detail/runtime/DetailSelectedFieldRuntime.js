sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailDirtyStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime"
], function (DetailDirtyStateRuntime, ModelStateRuntime, ModelContracts, DetailRowBindingRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;

    function normalizeEventValue(oEvent, sParameterName, sPropertyName, oSource) {
        var vValue = oEvent && oEvent.getParameter && oEvent.getParameter(sParameterName);
        if (typeof vValue !== "undefined") {
            return vValue;
        }
        if (oSource && typeof oSource.getProperty === "function") {
            return oSource.getProperty(sPropertyName);
        }
        return undefined;
    }

    function applySelectedFieldChange(oController, oEvent, mOptions, mHooks) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var sProperty = String((mOptions && mOptions.property) || "value").trim() || "value";
        var sParameter = String((mOptions && mOptions.parameter) || sProperty).trim() || sProperty;
        var sPath = DetailRowBindingRuntime.resolveSelectedBindingPath(oSource, sProperty);
        var vValue;
        if (!oSource || !sPath || !mHooks.isDirtyTrackMode()) {
            return false;
        }
        vValue = normalizeEventValue(oEvent, sParameter, sProperty, oSource);
        if (typeof vValue === "undefined") {
            return false;
        }
        ModelStateRuntime.write(oController, SELECTED_MODEL, sPath, vValue);
        DetailDirtyStateRuntime.markDirty(oController);
        return true;
    }

    function resolveRowInput(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return DetailRowBindingRuntime.resolveSelectedRowContext(oSource);
    }

    return {
        applySelectedFieldChange: applySelectedFieldChange,
        resolveRowInput: resolveRowInput
    };
});
