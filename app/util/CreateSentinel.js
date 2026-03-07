sap.ui.define([], function () {
    "use strict";

    var VALUE = "__CREATE";

    function normalize(vId) {
        var s = String(vId || "").trim();
        return s ? s.toUpperCase() : "";
    }

    function isCreateId(vId) {
        var s = normalize(vId);
        return s === VALUE || s === "__CREATE" || s === "__CREATE__" || s === "NEW";
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
