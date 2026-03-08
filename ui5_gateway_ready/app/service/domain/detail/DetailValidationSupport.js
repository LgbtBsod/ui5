sap.ui.define([
    "checklist/app/util/ValidationPathMap"
], function (ValidationPathMap) {
    "use strict";

    return {
        toMissingMap: ValidationPathMap.toMissingMap,
        toValidationKey: ValidationPathMap.toValidationKey
    };
});
