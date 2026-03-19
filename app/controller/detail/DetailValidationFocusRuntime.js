sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationHelperRuntime"
], function (FocusRuntime, ModelStateRuntime, SchedulingRuntime, ModelContracts, DetailValidationHelperRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;

    function resolveFocusDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl.getFocusDomRef === "function") {
            return oControl.getFocusDomRef() || null;
        }
        if (typeof oControl.getDomRef === "function") {
            return oControl.getDomRef() || null;
        }
        return null;
    }

    function scrollInvalidIntoView(oControl) {
        var oDomRef = resolveFocusDomRef(oControl);
        if (!oDomRef || typeof oDomRef.scrollIntoView !== "function") {
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
            return !!(oControl && oControl.data && oControl.data("validationKey"));
        });
        oTarget = aMissingKeys.reduce(function (oFound, sKey) {
            if (oFound) {
                return oFound;
            }
            return aControls.find(function (oControl) {
                return oControl && oControl.data && oControl.data("validationKey") === sKey;
            }) || null;
        }, null);
        if (!oTarget) {
            return false;
        }
        scrollInvalidIntoView(oTarget);
        if (typeof oTarget.focus === "function" && FocusRuntime.focusSoon(oTarget)) {
            return true;
        }
        var oDomRef = resolveFocusDomRef(oTarget);
        if (!oDomRef || typeof oDomRef.focus !== "function") {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oDomRef.focus();
        }, 0);
        return true;
    }

    return {
        focusFirstInvalidField: focusFirstInvalidField
    };
});
