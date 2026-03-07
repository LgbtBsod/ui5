sap.ui.define([], function () {
    "use strict";

    function rootId(mInput) {
        return String((mInput && (mInput.rootId || mInput.id)) || "").trim();
    }

    function text(vValue) {
        return String(vValue || "").trim();
    }

    function bool(vValue) {
        return !!vValue;
    }

    return {
        rootId: rootId,
        text: text,
        bool: bool
    };
});
