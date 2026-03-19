sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer"
], function (RuntimePayloadNormalizer) {
    "use strict";

    function normalizePayload(vCommandOrPayload, oPayload) {
        return RuntimePayloadNormalizer.normalize(
            arguments.length > 1 ? oPayload : vCommandOrPayload
        );
    }

    return Object.freeze({
        normalizePayload: normalizePayload
    });
});
