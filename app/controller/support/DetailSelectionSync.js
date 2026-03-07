sap.ui.define([
    "checklist/app/util/ValidationPathMap"
], function (ValidationPathMap) {
    "use strict";

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

        return {
            isFilledValidationValue: isFilledValidationValue,
            shouldTrackSelectedDirtyPath: shouldTrackSelectedDirtyPath,
            toValidationKey: ValidationPathMap.toValidationKey
        };
});
