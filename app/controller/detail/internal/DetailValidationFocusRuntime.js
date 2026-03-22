sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/internal/DetailValidationHelperRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (FocusRuntime, ModelStateRuntime, SchedulingRuntime, ModelContracts, DetailValidationHelperRuntime, JsRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function resolveFocusDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_FOCUS_DOM_REF]() || null;
        }
        if (typeof oControl[METHODS.GET_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_DOM_REF]() || null;
        }
        return null;
    }

    function scrollInvalidIntoView(oControl) {
        var oDomRef = resolveFocusDomRef(oControl);
        if (!oDomRef || typeof oDomRef.scrollIntoView !== TYPE_FUNCTION) {
            return false;
        }
        try {
            oDomRef.scrollIntoView({ behavior: "smooth", block: "center", inline: "nearest" });
            return true;
        } catch (_e) {
            try {
                oDomRef.scrollIntoView(true);
                return true;
            } catch (_e2) {
                return false;
            }
        }
    }

    function focusFirstInvalidField(oController, mStatePaths) {
        var sSummaryPath = DetailValidationHelperRuntime.validationSummaryPath(mStatePaths);
        var oSummary = ModelStateRuntime.read(oController, STATE_MODEL, sSummaryPath, {}) || {};
        var aMissingKeys = (oSummary && oSummary.missingKeys) || [];
        var oView = oController.getView && oController.getView();
        var aControls;
        var oTarget;
        if (!oView || !Array.isArray(aMissingKeys) || !aMissingKeys.length) {
            return false;
        }
        aControls = oView.findAggregatedObjects(true, function (oControl) {
            return !!(oControl && typeof oControl.data === TYPE_FUNCTION && oControl.data("validationKey"));
        });
        oTarget = aMissingKeys.reduce(function (oFound, sKey) {
            if (oFound) {
                return oFound;
            }
            return aControls.find(function (oControl) {
                return oControl && typeof oControl.data === TYPE_FUNCTION && oControl.data("validationKey") === sKey;
            }) || null;
        }, null);
        if (!oTarget) {
            return false;
        }
        scrollInvalidIntoView(oTarget);
        if (typeof oTarget[METHODS.FOCUS] === TYPE_FUNCTION && FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        var oDomRef = resolveFocusDomRef(oTarget);
        if (!oDomRef || typeof oDomRef[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oDomRef[METHODS.FOCUS]();
        }, 0);
        return true;
    }

    return {
        focusFirstInvalidField: focusFirstInvalidField
    };
});
