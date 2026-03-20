sap.ui.define([], function () {
    "use strict";

    var VALUE = "__CREATE";

    function normalize(vId) {
        var s = String(vId || "").trim();
        return s ? s.toUpperCase() : "";
    }

    function isCreateId(vId) {
        return normalize(vId) === VALUE;
    }

    function toRouteId() {
        return VALUE;
    }

    function toStateId() {
        return VALUE;
    }

    return {
        VALUE: VALUE,
        normalize: normalize,
        isCreateId: isCreateId,
        toRouteId: toRouteId,
        toStateId: toStateId
    };
});
