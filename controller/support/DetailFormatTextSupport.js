sap.ui.define([
    "sap_ui5/controller/base/ControllerTextRuntime"
], function (ControllerTextRuntime) {
    "use strict";

    var LIFECYCLE_TEXT_KEYS = {
        REGISTERED: { key: "statusRegistered", fallback: "Registered" },
        CLOSED: { key: "statusClosed", fallback: "Closed" },
        DRAFT: { key: "statusDraft", fallback: "Draft" }
    };
    var LIFECYCLE_STATES = {
        REGISTERED: "Warning",
        CLOSED: "Success",
        DRAFT: "Information"
    };
    var LOCK_OPERATION_STATES = {
        SUCCESS: "Success",
        ERROR: "Error"
    };

    function text(oController, sKey, vArgs, vFallback) {
        var aArgs = [];
        var sFallback = "";
        if (Array.isArray(vArgs)) {
            aArgs = vArgs;
            sFallback = typeof vFallback === "string" ? vFallback : "";
        } else if (typeof vArgs === "string") {
            sFallback = vArgs;
        } else if (typeof vFallback === "string") {
            sFallback = vFallback;
        }
        return ControllerTextRuntime.getText(oController, sKey, aArgs, sFallback);
    }

    function readMissingCount(oSummary) {
        if (!oSummary || typeof oSummary !== "object") {
            return 0;
        }
        return Number(
            oSummary.missingCount ||
            ((Array.isArray(oSummary.missingKeys) && oSummary.missingKeys.length) || 0)
        ) || 0;
    }

    return {
        formatValidationState: function (bShown, bMissing) {
            return (bShown && bMissing) ? "Error" : "None";
        },

        formatValidationText: function (bShown, bMissing) {
            return (bShown && bMissing) ? text(this, "requiredFieldHint") : "";
        },

        formatValidationSummaryText: function (oSummary) {
            var iCount = readMissingCount(oSummary);
            if (iCount > 0) {
                return text(this, "validationSummaryTitleCount", [iCount], text(this, "validationSummaryTitle"));
            }
            return text(this, "validationSummaryTitle");
        },

        formatValidationSummaryLinkText: function (oSummary) {
            var iCount = readMissingCount(oSummary);
            if (iCount > 1) {
                return text(this, "validationSummaryLinkMany", [iCount], text(this, "requiredFieldHint"));
            }
            if (iCount === 1) {
                return text(this, "validationSummaryLinkSingle", [], text(this, "requiredFieldHint"));
            }
            return text(this, "requiredFieldHint");
        },

        formatBooleanResultText: function (vValue) {
            if (vValue === true) {
                return text(this, "statusOk", "OK");
            }
            if (vValue === false) {
                return text(this, "statusFailed", "Failed");
            }
            return "-";
        },

        formatBooleanResultState: function (vValue) {
            if (vValue === true) {
                return "Success";
            }
            if (vValue === false) {
                return "Error";
            }
            return "None";
        },

        formatLifecycleStatusText: function (sStatus) {
            var sNormalized = String(sStatus || "").toUpperCase();
            var oMeta = LIFECYCLE_TEXT_KEYS[sNormalized] || LIFECYCLE_TEXT_KEYS.DRAFT;
            return text(this, oMeta.key, oMeta.fallback);
        },

        formatLifecycleStatusState: function (sStatus) {
            var sNormalized = String(sStatus || "").toUpperCase();
            return LIFECYCLE_STATES[sNormalized] || LIFECYCLE_STATES.DRAFT;
        },

        formatDraftStateText: function (bDirty) {
            return bDirty ? text(this, "detailDraftChanged") : text(this, "detailDraftClean");
        },

        formatDraftStateState: function (bDirty) {
            return bDirty ? "Warning" : "Success";
        },

        formatLockOperationText: function (sTextValue, sMode) {
            if (sTextValue) {
                return sTextValue;
            }
            return String(sMode || "").toUpperCase() === "EDIT"
                ? text(this, "modeEdit", "Edit")
                : text(this, "modeRead", "Read");
        },

        formatLockOperationState: function (sState, sMode) {
            var sNormalizedState = String(sState || "").toUpperCase();
            if (LOCK_OPERATION_STATES[sNormalizedState]) {
                return LOCK_OPERATION_STATES[sNormalizedState];
            }
            return String(sMode || "").toUpperCase() === "EDIT" ? "Success" : "Information";
        },

        formatI18nByKey: function (sKey) {
            return sKey ? text(this, sKey) : "";
        },

        formatHeartbeatText: function (sMode, sLockState) {
            return (sMode === "EDIT" && sLockState === "LOCKED")
                ? text(this, "heartbeatLockedActive")
                : text(this, "heartbeatInactive");
        },

        formatAutosaveText: function (sMode, sAutosaveState) {
            var sState = String(sAutosaveState || "IDLE").toUpperCase();
            var mKeyByState = {
                IDLE: "autosaveWaiting",
                SAVING: "autosaveSaving",
                SAVED: "autosaveSaved",
                ERROR: "autosaveError"
            };
            if (sMode !== "EDIT") {
                return text(this, "autosaveDisabled");
            }
            return text(this, mKeyByState[sState] || "autosaveWaiting");
        }
    };
});
