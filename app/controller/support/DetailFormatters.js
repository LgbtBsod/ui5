sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (ControllerTextRuntime, CreateSentinel) {
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
    var LOCK_ACTIVE_STATES = {
        EDIT_LOCKED: true
    };
    var LOCK_TRANSITION_STATES = {
        ACQUIRING_LOCK: true,
        IDLE_TIMEOUT_GRACE: true
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

    function parseDateLike(vValue) {
        var sValue;
        var oDate;
        if (vValue instanceof Date && !Number.isNaN(vValue.getTime())) {
            return vValue;
        }
        sValue = String(vValue || "").trim();
        if (!sValue) {
            return null;
        }
        if (/^\d{4}-\d{2}-\d{2}$/.test(sValue)) {
            oDate = new Date(sValue + "T00:00:00Z");
            return Number.isNaN(oDate.getTime()) ? null : oDate;
        }
        oDate = new Date(sValue);
        return Number.isNaN(oDate.getTime()) ? null : oDate;
    }

    function formatHumanDate(vValue) {
        var oDate = parseDateLike(vValue);
        if (!oDate) {
            return "";
        }
        return oDate.toLocaleDateString(undefined, {
            year: "numeric",
            month: "short",
            day: "2-digit"
        });
    }

    function formatDateTimeCardValue(sDate, sTime, sTimezone) {
        var sHumanDate = formatHumanDate(sDate) || "-";
        var sTimeValue = String(sTime || "-");
        var sTimezoneValue = String(sTimezone || "-");
        return [
            "Date: " + sHumanDate,
            "Time: " + sTimeValue,
            "Timezone: " + sTimezoneValue
        ].join("\n");
    }

    function formatAutosaveTime(vValue) {
        var oDate = parseDateLike(vValue);
        var sLanguageTag = "";
        if (!oDate) {
            return vValue || "-";
        }
        try {
            sLanguageTag = sap.ui.getCore().getConfiguration().getLanguageTag().toString();
        } catch (oError) {
            sLanguageTag = "";
        }
        try {
            return oDate.toLocaleTimeString(sLanguageTag || undefined, {
                hour: "2-digit",
                minute: "2-digit"
            });
        } catch (oLocaleError) {
            return oDate.toLocaleTimeString(undefined, {
                hour: "2-digit",
                minute: "2-digit"
            });
        }
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

        formatLockStateText: function (sTextValue, sMode) {
            if (sTextValue) {
                return sTextValue;
            }
            return String(sMode || "").toUpperCase() === "EDIT"
                ? text(this, "modeEdit", "Edit")
                : text(this, "modeRead", "Read");
        },

        formatLockStateSemantic: function (sState, sMode) {
            var sNormalizedState = String(sState || "").toUpperCase();
            if (LOCK_OPERATION_STATES[sNormalizedState]) {
                return LOCK_OPERATION_STATES[sNormalizedState];
            }
            if (LOCK_ACTIVE_STATES[sNormalizedState]) {
                return "Success";
            }
            if (sNormalizedState === "LOCK_LOST" || sNormalizedState === "FORCED_READ_ONLY") {
                return "Error";
            }
            if (LOCK_TRANSITION_STATES[sNormalizedState]) {
                return "Warning";
            }
            return String(sMode || "").toUpperCase() === "EDIT" ? "Warning" : "Information";
        },

        formatI18nByKey: function (sKey) {
            return sKey ? text(this, sKey) : "";
        },

        formatHeartbeatText: function (sMode, sLockState) {
            var sNormalizedLockState = String(sLockState || "").toUpperCase();
            if (String(sMode || "").toUpperCase() === "EDIT" && LOCK_ACTIVE_STATES[sNormalizedLockState]) {
                return text(this, "heartbeatLockedActive");
            }
            if (sNormalizedLockState === "ACQUIRING_LOCK") {
                return text(this, "autosaveSaving", "Saving...");
            }
            if (sNormalizedLockState === "IDLE_TIMEOUT_GRACE") {
                return text(this, "idleTimeoutGraceBannerTitle", "Idle timeout warning");
            }
            if (sNormalizedLockState === "LOCK_LOST") {
                return text(this, "lockLostBannerTitle", "Lock lost");
            }
            if (sNormalizedLockState === "FORCED_READ_ONLY") {
                return text(this, "detailForcedReadOnlyTitle", "Read-only enforced");
            }
            return text(this, "heartbeatInactive");
        },

        formatAutosaveText: function (sMode, sLockState, sAutosaveState) {
            var sState = String(sAutosaveState || "IDLE").toUpperCase();
            var mKeyByState = {
                IDLE: "autosaveWaiting",
                SAVING: "autosaveSaving",
                SAVED: "autosaveSaved",
                ERROR: "autosaveError"
            };
            var sNormalizedMode = String(sMode || "").toUpperCase();
            var sNormalizedLockState = String(sLockState || "").toUpperCase();
            if (sNormalizedMode !== "EDIT" || !LOCK_ACTIVE_STATES[sNormalizedLockState]) {
                return text(this, "autosaveDisabled");
            }
            return text(this, mKeyByState[sState] || "autosaveWaiting");
        },

        formatPassedTotal: function (aRows) {
            if (!Array.isArray(aRows) || !aRows.length) {
                return "";
            }
            return aRows.filter(function (oRow) {
                return !!(oRow && oRow.result);
            }).length + "/" + aRows.length;
        },

        formatHeaderDate: function (sDate) {
            return formatHumanDate(sDate) || sDate || "-";
        },

        formatInfoCardValue: function (sKey, sDate, sTime, sTimezone, sEquipment, _sObserver, _sObserved, sLocationName, sLocationText, sLpcText, sProfText) {
            var mByKey = {
                datetime: formatDateTimeCardValue(sDate, sTime, sTimezone),
                equipment: [sEquipment],
                location: [sLocationName || sLocationText],
                lpc: [sLpcText],
                profession: [sProfText]
            };
            if (sKey === "datetime") {
                return mByKey[sKey] || "-";
            }
            return (mByKey[sKey] || []).filter(Boolean).join(" | ") || "-";
        },

        formatPersonSuggestion: function (sName, sPosition, sPerner, sOrgUnit) {
            return [sName, sPosition, sPerner, sOrgUnit].filter(Boolean).join(" | ");
        },

        formatPersonSuggestionMeta: function (sPerner, sPosition, sOrgUnit) {
            return [sPerner, sPosition, sOrgUnit].filter(Boolean).join(" | ");
        },

        formatAttachmentSize: function (nBytes) {
            var nValue = Number(nBytes || 0) || 0;
            if (nValue < 1024) {
                return nValue + " B";
            }
            if (nValue < 1024 * 1024) {
                return (nValue / 1024).toFixed(1) + " KB";
            }
            return (nValue / (1024 * 1024)).toFixed(1) + " MB";
        },

        formatAttachmentCategoryText: function (sCategoryKey, aAttachmentTypes, sCategoryText) {
            var sKey = String(sCategoryKey || "").trim();
            var aItems = Array.isArray(aAttachmentTypes) ? aAttachmentTypes : [];
            var oFound = aItems.find(function (oItem) {
                return String((oItem && oItem.key) || "").trim() === sKey;
            });
            return (oFound && oFound.text) || sCategoryText || sKey || "-";
        },

        formatAutosaveAt: function (sAutosaveAt) {
            return formatAutosaveTime(sAutosaveAt);
        },

        formatCopyLinkVisible: function (sActiveObjectId) {
            return !!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId);
        },

        formatDeleteChecklistVisible: function (sMode, sActiveObjectId) {
            return String(sMode || "").toUpperCase() === "EDIT" &&
                !!sActiveObjectId &&
                !CreateSentinel.isCreateId(sActiveObjectId);
        }
    };
});
