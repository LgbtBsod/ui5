sap.ui.define([], function () {
    "use strict";

    var KINDS = {
        NETWORK: "NETWORK",
        LOCK: "LOCK",
        CONFLICT: "CONFLICT",
        VALIDATION: "VALIDATION",
        BACKEND: "BACKEND",
        UNKNOWN: "UNKNOWN"
    };

    function create(mPartial) {
        var oPartial = mPartial || {};
        return {
            kind: oPartial.kind || KINDS.UNKNOWN,
            code: oPartial.code || "UNKNOWN",
            messageKey: oPartial.messageKey || "unexpectedError",
            params: oPartial.params || {},
            severity: oPartial.severity || "error",
            retriable: !!oPartial.retriable
        };
    }

    return {
        KINDS: KINDS,
        create: create
    };
});
