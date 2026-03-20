sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        INFO_CARD_KEYS: Object.freeze({
            DATETIME: "datetime",
            EQUIPMENT: "equipment",
            LOCATION: "location",
            LPC: "lpc",
            OBSERVED: "observed",
            OBSERVER: "observer",
            PROFESSION: "profession"
        }),
        INFO_CARD_TEXT_KEYS: Object.freeze({
            DATETIME: "dateTimeBlockLabel",
            EQUIPMENT: "equipmentLabel",
            LOCATION: "locationLabel",
            LPC: "lpcLabel",
            OBSERVED: "observedLabel",
            OBSERVER: "observerLabel",
            PROFESSION: "professionLabel"
        }),
        INFO_CARD_TEXT_FALLBACKS: Object.freeze({
            DATETIME: "Date & Time",
            EQUIPMENT: "Equipment",
            LOCATION: "Location",
            LPC: "LPC",
            OBSERVED: "Observed",
            OBSERVER: "Observer",
            PROFESSION: "Profession"
        }),
        CARD_REQUIRED_KEYS: Object.freeze({
            datetime: Object.freeze(["basic.date", "basic.time", "basic.timezone"]),
            equipment: Object.freeze(["basic.equipment"]),
            observer: Object.freeze(["basic.OBSERVER_FULLNAME"]),
            observed: Object.freeze(["basic.OBSERVED_FULLNAME"]),
            location: Object.freeze(["basic.LOCATION_KEY"]),
            lpc: Object.freeze(["basic.LPC_KEY"]),
            profession: Object.freeze(["basic.PROF_KEY"])
        }),
        VIEW_DEFAULTS: Object.freeze({
            ATTACHMENT_CATEGORY_KEY: "GEN"
        }),
        ATTACHMENTS: Object.freeze({
            HINT_FALLBACK_EMPTY_TOKEN: "-",
            HINT_FALLBACK_SEPARATOR: " - ",
            HINT_TEXT_KEY: "attachmentUploadHint",
            MIME_REJECTED_TOAST_KEY: "attachmentMimeRejected",
            SIZE_REJECTED_TOAST_KEY: "attachmentSizeRejected",
            UPLOAD_BUSY_PATH: "/attachmentBusy",
            UPLOAD_DISABLED_TOAST_KEY: "attachmentUploadDisabled"
        })
    });
});
