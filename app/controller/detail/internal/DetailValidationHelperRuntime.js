sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/ValidationPathMap"
], function (ControllerViewStateRuntime, ValidationPathMap) {
    "use strict";

    function validationSummaryPath(mStatePaths) {
        return (mStatePaths && mStatePaths.VALIDATION_SUMMARY) || "/validationSummary";
    }

    function setValidationMissing(oController, vValue) {
        ControllerViewStateRuntime.set(oController, "/validationMissing", vValue || {});
    }

    function setValidationMissingKey(oController, sKey, bMissing) {
        ControllerViewStateRuntime.set(oController, "/validationMissing/" + sKey, !!bMissing);
    }

    function showValidation(oController, bShowValidation) {
        if (bShowValidation || ControllerViewStateRuntime.get(oController, "/validationShown")) {
            ControllerViewStateRuntime.set(oController, "/validationShown", true);
        }
    }

    function isFilledValidationValue(vValue) {
        if (Array.isArray(vValue)) {
            return vValue.length > 0;
        }
        if (typeof vValue === "boolean") {
            return true;
        }
        return String(vValue == null ? "" : vValue).trim().length > 0;
    }

    function shouldTrackSelectedDirtyPath(sModelPath) {
        var sPath = "/" + String(sModelPath || "").replace(/^\//, "");
        if (sPath === "/") {
            return false;
        }
        if (/^\/attachments(?:\/|$)/.test(sPath)) {
            return false;
        }
        if (/^\/(?:root|meta)(?:\/|$)/.test(sPath)) {
            return false;
        }
        return /^\/(?:basic|checks|barriers)(?:\/|$)/.test(sPath);
    }

    function toValidationKey(sRequiredPath) {
        return ValidationPathMap.toValidationKey(sRequiredPath);
    }

    return {
        isFilledValidationValue: isFilledValidationValue,
        setValidationMissing: setValidationMissing,
        setValidationMissingKey: setValidationMissingKey,
        shouldTrackSelectedDirtyPath: shouldTrackSelectedDirtyPath,
        showValidation: showValidation,
        toValidationKey: toValidationKey,
        validationSummaryPath: validationSummaryPath
    };
});
