sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (RuntimeInput, FacadeCommandConstants) {
    "use strict";

    var DETAIL_METHODS = FacadeCommandConstants.DETAIL;
    var SEARCH_METHODS = FacadeCommandConstants.SEARCH;

    function normalizeKnownMethod(vMethod, mKnown) {
        var sMethod = RuntimeInput.asString(vMethod).trim();
        if (!sMethod) {
            return "";
        }
        return Object.keys(mKnown).some(function (sKey) {
            return mKnown[sKey] === sMethod;
        }) ? sMethod : sMethod;
    }

    return {
        DETAIL_METHODS: DETAIL_METHODS,
        SEARCH_METHODS: SEARCH_METHODS,
        normalizeKnownMethod: normalizeKnownMethod
    };
});
