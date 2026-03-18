sap.ui.define([], function () {
    "use strict";

    var EDIT_MODE = {
        CREATE: "C",
        UPDATE: "U",
        DELETE: "D"
    };

    var NODES = {
        ROOT: "root",
        CHECKS: "checks",
        BARRIERS: "barriers",
        PARTICIPANTS: "participants",
        ATTACHMENTS: "attachments"
    };

    function normalizeEditMode(vMode, sFallback) {
        var sMode = String(vMode || "").trim().toUpperCase();
        if (sMode === EDIT_MODE.CREATE || sMode === EDIT_MODE.UPDATE || sMode === EDIT_MODE.DELETE) {
            return sMode;
        }
        return sFallback || EDIT_MODE.UPDATE;
    }

    return {
        EDIT_MODE: EDIT_MODE,
        NODES: NODES,
        normalizeEditMode: normalizeEditMode
    };
});
