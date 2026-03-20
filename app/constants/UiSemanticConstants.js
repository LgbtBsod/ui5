sap.ui.define([], function () {
    "use strict";

    var VALUE_STATE = Object.freeze({
        NONE: "None",
        INFORMATION: "Information",
        SUCCESS: "Success",
        WARNING: "Warning",
        ERROR: "Error"
    });

    var MESSAGE_TYPE = Object.freeze({
        INFORMATION: "Information",
        SUCCESS: "Success",
        WARNING: "Warning",
        ERROR: "Error"
    });

    return Object.freeze({
        OBJECT_STATUS_STATE: VALUE_STATE,
        VALUE_STATE: VALUE_STATE,
        MESSAGE_TYPE: MESSAGE_TYPE
    });
});
