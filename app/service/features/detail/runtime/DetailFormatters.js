sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailPersistenceConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (EffectTextResolver, CreateSentinel, WorkflowContracts, Ui5RuntimeFacade, DetailPersistenceConstants, UiSemanticConstants) {
    "use strict";

    var CHECKLIST_STATUSES = WorkflowContracts.CHECKLIST_STATUSES;
    var LIFECYCLE_TEXT_KEYS = {};
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.REGISTERED] = { key: "statusRegistered", fallback: "Registered" };
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.CLOSED] = { key: "statusClosed", fallback: "Closed" };
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.DRAFT] = { key: "statusDraft", fallback: "Draft" };
    var LIFECYCLE_STATES = {};
    LIFECYCLE_STATES[CHECKLIST_STATUSES.REGISTERED] = UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
    LIFECYCLE_STATES[CHECKLIST_STATUSES.CLOSED] = UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
    LIFECYCLE_STATES[CHECKLIST_STATUSES.DRAFT] = UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    var LOCK_STATE_SEMANTICS = {
        SUCCESS: UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS,
        ERROR: UiSemanticConstants.OBJECT_STATUS_STATE.ERROR
    };
    var LOCK_ACTIVE_STATES = {
        EDIT_LOCKED: true
    };
    var LOCK_TRANSITION_STATES = {
        ACQUIRING_LOCK: true,
        IDLE_TIMEOUT_GRACE: true
    };
    var AUTOSAVE_TEXT_FALLBACKS = {
        autosaveWaiting: "Autosave waiting",
        autosaveSaving: "Saving...",
        autosaveSaved: "All changes synced",
        autosaveError: "Autosave error",
        autosaveDisabled: "Autosave disabled (read-only mode)"
    };
    var PERSISTENCE_SEMANTICS = {
        saving: UiSemanticConstants.OBJECT_STATUS_STATE.WARNING,
        autosaving: UiSemanticConstants.OBJECT_STATUS_STATE.WARNING,
        saved: UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS,
        dirty: UiSemanticConstants.OBJECT_STATUS_STATE.WARNING,
        error: UiSemanticConstants.OBJECT_STATUS_STATE.ERROR,
        lockLost: UiSemanticConstants.OBJECT_STATUS_STATE.ERROR,
        readOnly: UiSemanticConstants.OBJECT_STATUS_STATE.WARNING,
        idleTimeoutGrace: UiSemanticConstants.OBJECT_STATUS_STATE.WARNING,
        conflict: UiSemanticConstants.OBJECT_STATUS_STATE.ERROR,
        idle: UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION
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
        return EffectTextResolver.getText(oController, sKey, aArgs, sFallback);
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
        var aParts;
        if (vValue instanceof Date && !Number.isNaN(vValue.getTime())) {
            return vValue;
        }
        sValue = String(vValue || "").trim();
        if (!sValue) {
            return null;
        }
        if (/^\d{4}-\d{2}-\d{2}$/.test(sValue)) {
            aParts = sValue.split("-");
            oDate = new Date(
                Number(aParts[0]),
                Number(aParts[1]) - 1,
                Number(aParts[2]),
                0,
                0,
                0,
                0
            );
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
        if (!oDate) {
            return vValue || "-";
        }
        var sLanguageTag = Ui5RuntimeFacade.getLanguageTag();
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
            return (bShown && bMissing) ? UiSemanticConstants.VALUE_STATE.ERROR : UiSemanticConstants.VALUE_STATE.NONE;
        },

        formatValidationText: function (bShown, bMissing) {
            return (bShown && bMissing) ? text(this, "requiredFieldHint") : "";
        },

        formatWarningMessageType: function () {
            return UiSemanticConstants.MESSAGE_TYPE.WARNING;
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
            if (typeof vValue === "string") {
                var sValue = String(vValue).trim().toUpperCase();
                if (sValue === "OK" || sValue === "TRUE" || sValue === "X") {
                    return text(this, "statusOk", "OK");
                }
                if (sValue === "FAILED" || sValue === "ERROR" || sValue === "FALSE") {
                    return text(this, "statusFailed", "Failed");
                }
                return "-";
            }
            if (vValue === true) {
                return text(this, "statusOk", "OK");
            }
            if (vValue === false) {
                return text(this, "statusFailed", "Failed");
            }
            return "-";
        },

        formatBooleanResultState: function (vValue) {
            if (typeof vValue === "string") {
                var sValue = String(vValue).trim().toUpperCase();
                if (sValue === "OK" || sValue === "TRUE" || sValue === "X") {
                    return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
                }
                if (sValue === "FAILED" || sValue === "ERROR" || sValue === "FALSE") {
                    return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
                }
                return UiSemanticConstants.OBJECT_STATUS_STATE.NONE;
            }
            if (vValue === true) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
            }
            if (vValue === false) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
            }
            return UiSemanticConstants.OBJECT_STATUS_STATE.NONE;
        },

        formatLifecycleStatusText: function (sStatus) {
            var sNormalized = String(sStatus || "").toUpperCase();
            var oMeta = LIFECYCLE_TEXT_KEYS[sNormalized] || LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.DRAFT];
            return text(this, oMeta.key, oMeta.fallback);
        },

        formatLifecycleStatusState: function (sStatus) {
            var sNormalized = String(sStatus || "").toUpperCase();
            return LIFECYCLE_STATES[sNormalized] || LIFECYCLE_STATES[CHECKLIST_STATUSES.DRAFT];
        },

        formatDraftStateText: function (bDirty) {
            return bDirty ? text(this, "detailDraftChanged") : text(this, "detailDraftClean");
        },

        formatDraftStateState: function (bDirty) {
            return bDirty ? UiSemanticConstants.OBJECT_STATUS_STATE.WARNING : UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        },

        formatLockStateText: function (sTextValue, sMode) {
            if (sTextValue) {
                return sTextValue;
            }
            return WorkflowContracts.isEditableMode(sMode)
                ? text(this, "modeEdit", "Edit")
                : text(this, "modeRead", "Read");
        },

        formatLockStateSemantic: function (sState, sMode) {
            var sNormalizedState = String(sState || "").toUpperCase();
            if (LOCK_STATE_SEMANTICS[sNormalizedState]) {
                return LOCK_STATE_SEMANTICS[sNormalizedState];
            }
            if (LOCK_ACTIVE_STATES[sNormalizedState]) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
            }
            if (sNormalizedState === "LOCK_LOST" || sNormalizedState === "FORCED_READ_ONLY") {
                return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
            }
            if (LOCK_TRANSITION_STATES[sNormalizedState]) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
            }
            return WorkflowContracts.isEditableMode(sMode) ? UiSemanticConstants.OBJECT_STATUS_STATE.WARNING : UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
        },

        formatI18nByKey: function (sKey) {
            return sKey ? text(this, sKey) : "";
        },

        formatHeartbeatText: function (sMode, sLockState) {
            var sNormalizedLockState = String(sLockState || "").toUpperCase();
            if (WorkflowContracts.isEditableMode(sMode) && LOCK_ACTIVE_STATES[sNormalizedLockState]) {
                return text(this, "heartbeatLockedActive", "Heartbeat active");
            }
            if (WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.CREATE) {
                return text(this, "heartbeatDraftCreate", "Draft editing active");
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
            return text(this, "heartbeatInactive", "Heartbeat paused (read-only mode)");
        },

        formatAutosaveText: function (sMode, sLockState, sAutosaveState) {
            var sState = WorkflowContracts.normalizeAutosaveState(sAutosaveState);
            var mKeyByState = {
                IDLE: "autosaveWaiting",
                SAVING: "autosaveSaving",
                SAVED: "autosaveSaved",
                ERROR: "autosaveError"
            };
            var sNormalizedMode = WorkflowContracts.normalizeEditMode(sMode);
            var sNormalizedLockState = String(sLockState || "").toUpperCase();
            if (sNormalizedMode === WorkflowContracts.EDIT_MODES.CREATE) {
                return text(this, "autosaveCreateDraft", "Draft changes are local until first save");
            }
            if (sNormalizedMode !== WorkflowContracts.EDIT_MODES.EDIT || !LOCK_ACTIVE_STATES[sNormalizedLockState]) {
                return text(this, "autosaveDisabled", "Autosave disabled (read-only mode)");
            }
            return text(this, mKeyByState[sState] || "autosaveWaiting", AUTOSAVE_TEXT_FALLBACKS[mKeyByState[sState] || "autosaveWaiting"] || "Autosave waiting");
        },

        formatPersistenceText: function (sMessageKey, sPersistenceState, sLastSavedAt) {
            if (sMessageKey) {
                if (sMessageKey === "persistenceSaved" || sMessageKey === "persistenceAutosaveSaved") {
                    if (sLastSavedAt) {
                        return text(this, "persistenceSavedAt", [formatAutosaveTime(sLastSavedAt)], "Saved");
                    }
                }
                return text(this, sMessageKey, sMessageKey);
            }
            if (sPersistenceState === DetailPersistenceConstants.STATES.SAVED && sLastSavedAt) {
                return text(this, "persistenceSavedAt", [formatAutosaveTime(sLastSavedAt)], "Saved");
            }
            return text(this, "persistenceIdle", "No pending changes");
        },

        formatPersistenceTooltip: function (sMessageKey, sPersistenceState, sLastSavedAt, oLastSaveError) {
            var sPrimary = this.formatPersistenceText(sMessageKey, sPersistenceState, sLastSavedAt);
            var sCode = String((oLastSaveError && oLastSaveError.code) || "").trim();
            var sMessage = String((oLastSaveError && oLastSaveError.message) || "").trim();
            if (!sCode && !sMessage) {
                return sPrimary;
            }
            return text(this, "persistenceTooltipWithDetails", [sPrimary, [sCode, sMessage].filter(Boolean).join(": ")], sPrimary);
        },

        formatPersistenceState: function (sPersistenceState) {
            return PERSISTENCE_SEMANTICS[String(sPersistenceState || "").trim()] || "Information";
        },

        formatPassedTotal: function (aRows) {
            if (!Array.isArray(aRows) || !aRows.length) {
                return "";
            }
            return aRows.filter(function (oRow) {
                return !!(oRow && oRow.result);
            }).length + "/" + aRows.length;
        },

        formatRowCount: function (aRows) {
            return Array.isArray(aRows) ? String(aRows.length) : "0";
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
            return WorkflowContracts.isEditableMode(sMode) &&
                !!sActiveObjectId &&
                !CreateSentinel.isCreateId(sActiveObjectId);
        },

        formatAttachmentsEmptyStateText: function (sRootId) {
            return CreateSentinel.isCreateId(sRootId)
                ? text(this, "attachmentDraftStageHint")
                : text(this, "detailEmptyAttachmentsSavedText", text(this, "detailEmptyAttachmentsText"));
        }
    };
});
