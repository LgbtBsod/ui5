sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        AUTOSAVE_FIELDS: Object.freeze({
            BARRIERS_NUMBER: "BARRIERS_NUMBER",
            CHECKS_NUMBER: "CHECKS_NUMBER",
            LPC_KEY: "LPC_KEY",
            PROF_KEY: "PROF_KEY"
        }),
        TEXT_PATHS: Object.freeze({
            BARRIERS_NUMBER: "/basic/BARRIERS_NUMBER_TEXT",
            CHECKS_NUMBER: "/basic/CHECKS_NUMBER_TEXT",
            LPC: "/basic/LPC_TEXT",
            PROFESSION: "/basic/PROF_TEXT"
        }),
        VALUE_HELP_DIALOGS: Object.freeze({
            LOCATION: "locationValueHelp"
        }),
        VIEW_PATHS: Object.freeze({
            ATTACHMENT_BUSY: "/attachmentBusy",
            ATTACHMENTS_EXPANDED: "/attachmentsExpanded",
            ATTACHMENTS_LOADED: "/attachmentsLoaded",
            LOCATION_VH_BUSY: "/locationVhBusy"
        })
    });
});
