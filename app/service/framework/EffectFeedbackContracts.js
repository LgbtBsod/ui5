sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (FeedbackConstants) {
    "use strict";

    return Object.freeze({
        CLASSES: Object.freeze({
            DIALOG: "appDialogSurface",
            TOAST: "glassToast",
            TOAST_LEVEL_PREFIX: "glassToast--"
        }),
        DURATIONS: Object.freeze({
            TOAST_DEDUPE_MS: 2500,
            TOAST_SHOW_MS: 2200
        }),
        EFFECT_HANDLER_NAMES: Object.freeze({
            BANNER: "banner",
            DIALOG: "dialog"
        }),
        FALLBACK_TEXT_KEYS: Object.freeze({
            CONFLICT_DIALOG: "conflictDialogText",
            LOAD_ERROR: "loadErrorMessage"
        }),
        IDS: Object.freeze({
            CONFLICT: "conflict"
        }),
        LEVELS: Object.freeze({
            INFO: FeedbackConstants.SEVERITY.INFO
        }),
        VARIANTS: Object.freeze({
            INFORMATION: FeedbackConstants.VARIANT.INFORMATION,
            WARNING: FeedbackConstants.VARIANT.WARNING
        })
    });
});
