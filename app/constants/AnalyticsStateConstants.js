sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (UiSemanticConstants) {
    "use strict";

    var LOAD_STATUS = Object.freeze({
        ERROR: "error",
        LOADING: "loading",
        READY: "ready"
    });

    var REFRESH_STATUS = Object.freeze({
        ERROR: "ERROR",
        IDLE: "IDLE",
        READY: "READY",
        REQUESTED: "REQUESTED",
        RUNNING: "RUNNING",
        SUCCESS: "SUCCESS"
    });

    var REFRESH_MESSAGE_TYPE = Object.freeze({
        ACTIVE: UiSemanticConstants.MESSAGE_TYPE.INFORMATION,
        ERROR: UiSemanticConstants.MESSAGE_TYPE.WARNING,
        INFO: UiSemanticConstants.MESSAGE_TYPE.INFORMATION
    });

    return Object.freeze({
        LOAD_STATUS: LOAD_STATUS,
        REFRESH_STATUS: REFRESH_STATUS,
        REFRESH_MESSAGE_TYPE: REFRESH_MESSAGE_TYPE
    });
});
