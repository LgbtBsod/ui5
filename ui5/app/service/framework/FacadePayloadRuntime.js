sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function normalizeKnownMethod(vMethod, mKnown) {
        var sMethod = RuntimeInput.asString(vMethod).trim();
        if (!sMethod) {
            return "";
        }
        return Object.keys(mKnown).some(function (sKey) {
            return mKnown[sKey] === sMethod;
        }) ? sMethod : sMethod;
    }

    function normalizePayload(vCommandOrPayload, oPayload) {
        var oResolvedPayload = arguments.length > 1 ? oPayload : vCommandOrPayload;
        var oInput = RuntimeInput.asObject(oResolvedPayload);
        var oNormalized = Object.assign({}, oInput);
        ["rootId", "selectedRowId", "intent", "source", "entity", "op", "field", "key"].forEach(function (sKey) {
            if (Object.prototype.hasOwnProperty.call(oInput, sKey)) {
                oNormalized[sKey] = RuntimeInput.asString(oInput[sKey]).trim();
            }
        });
        ["state", "silent", "userInitiated"].forEach(function (sKey) {
            if (Object.prototype.hasOwnProperty.call(oInput, sKey)) {
                oNormalized[sKey] = RuntimeInput.asBoolean(oInput[sKey], false);
            }
        });
        return oNormalized;
    }

    return {
        normalizeKnownMethod: normalizeKnownMethod,
        normalizePayload: normalizePayload
    };
});
