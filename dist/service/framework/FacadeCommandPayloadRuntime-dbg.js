sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function normalizePayload(vCommandOrPayload, oPayload) {
        var oResolvedPayload = arguments.length > 1 ? oPayload : vCommandOrPayload;
        var oInput = RuntimeInput.asObject(oResolvedPayload);
        var oNormalized = Object.assign({}, oInput);
        if (Object.prototype.hasOwnProperty.call(oInput, "rootId")) {
            oNormalized.rootId = RuntimeInput.asString(oInput.rootId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "selectedRowId")) {
            oNormalized.selectedRowId = RuntimeInput.asString(oInput.selectedRowId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "intent")) {
            oNormalized.intent = RuntimeInput.asString(oInput.intent).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "source")) {
            oNormalized.source = RuntimeInput.asString(oInput.source).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "entity")) {
            oNormalized.entity = RuntimeInput.asString(oInput.entity).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "op")) {
            oNormalized.op = RuntimeInput.asString(oInput.op).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "field")) {
            oNormalized.field = RuntimeInput.asString(oInput.field).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "key")) {
            oNormalized.key = RuntimeInput.asString(oInput.key).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "state")) {
            oNormalized.state = RuntimeInput.asBoolean(oInput.state, false);
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "silent")) {
            oNormalized.silent = RuntimeInput.asBoolean(oInput.silent, false);
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "userInitiated")) {
            oNormalized.userInitiated = RuntimeInput.asBoolean(oInput.userInitiated, false);
        }
        return oNormalized;
    }

    return {
        normalizePayload: normalizePayload
    };
});
