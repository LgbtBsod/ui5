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
            BARRIERS_NUMBER: "barriersNumberValueHelp",
            CHECKS_NUMBER: "checksNumberValueHelp",
            LOCATION: "locationValueHelp"
        }),
        VIEW_PATHS: Object.freeze({
            ATTACHMENT_BUSY: "/attachmentBusy",
            ATTACHMENT_ACTIONS_ENABLED: "/attachmentActionsEnabled",
            ATTACHMENT_META_EDITABLE: "/attachmentMetaEditable",
            ATTACHMENT_DESKTOP_COLUMNS_VISIBLE: "/attachmentDesktopColumnsVisible",
            ATTACHMENT_ACTIONS_COLUMN_WIDTH: "/attachmentActionsColumnWidth",
            ATTACHMENT_CATEGORY_KEY: "/attachmentCategoryKey",
            ATTACHMENTS_EXPANDED: "/attachmentsExpanded",
            ATTACHMENTS_LOADED: "/attachmentsLoaded",
            SESSION_ATTACHMENTS: "/sessionAttachments",
            SHOW_SESSION_ATTACHMENTS: "/showSessionAttachments",
            LOCATION_VH_BUSY: "/locationVhBusy"
        })
    });
});
