sap.ui.define([], function () {
    "use strict";

    var VALUE = "__CREATE";
    var LEGACY_VALUES = Object.freeze([
        VALUE,
        "__CREATE__",
        "NEW"
    ]);

    function normalize(vId) {
        var s = String(vId || "").trim();
        return s ? s.toUpperCase() : "";
    }

    function isCreateId(vId) {
        var s = normalize(vId);
        return LEGACY_VALUES.indexOf(s) >= 0;
    }

    function toRouteId() {
        return VALUE;
    }

    function toStateId() {
        return VALUE;
    }

    return {
        VALUE: VALUE,
        LEGACY_VALUES: LEGACY_VALUES,
        normalize: normalize,
        isCreateId: isCreateId,
        toRouteId: toRouteId,
        toStateId: toStateId
    };
});
