sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (UiSemanticConstants) {
    "use strict";

    var SEVERITY = Object.freeze({
        INFO: "info",
        SUCCESS: "success",
        WARNING: "warning",
        ERROR: "error"
    });

    var SCOPE = Object.freeze({
        GLOBAL: "global",
        ROUTE: "route"
    });

    var VARIANT = Object.freeze({
        INFORMATION: "information",
        WARNING: "warning"
    });

    var MESSAGE_TYPE_BY_SEVERITY = Object.freeze({
        info: UiSemanticConstants.MESSAGE_TYPE.INFORMATION,
        success: UiSemanticConstants.MESSAGE_TYPE.SUCCESS,
        warning: UiSemanticConstants.MESSAGE_TYPE.WARNING,
        error: UiSemanticConstants.MESSAGE_TYPE.ERROR
    });

    return Object.freeze({
        SEVERITY: SEVERITY,
        SCOPE: SCOPE,
        VARIANT: VARIANT,
        MESSAGE_TYPE_BY_SEVERITY: MESSAGE_TYPE_BY_SEVERITY
    });
});
