sap.ui.define([], function () {
    "use strict";

    var ENTITY_CONFIG = Object.freeze({
        check: Object.freeze({
            key: "check",
            rowsPath: "/checks",
            numberField: "ChecksNum",
            rowBusyPath: "/checksBusy",
            dialogBusyPath: "/checksExpandedBusy",
            dialogId: "checksExpanded"
        }),
        barrier: Object.freeze({
            key: "barrier",
            rowsPath: "/barriers",
            numberField: "BarriersNum",
            rowBusyPath: "/barriersBusy",
            dialogBusyPath: "/barriersExpandedBusy",
            dialogId: "barriersExpanded"
        })
    });

    function get(sEntity) {
        return ENTITY_CONFIG[String(sEntity || "").trim()] || ENTITY_CONFIG.check;
    }

    return Object.freeze({
        all: ENTITY_CONFIG,
        get: get
    });
});
