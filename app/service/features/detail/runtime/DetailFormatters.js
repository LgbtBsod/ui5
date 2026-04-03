sap.ui.define([
    "sap/ui/core/format/DateFormat",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (DateFormat, ControllerTextRuntime, CreateSentinel, WorkflowContracts, MessageKeyConstants, UiSemanticConstants) {
    "use strict";

    var CHECKLIST_STATUSES = WorkflowContracts.CHECKLIST_STATUSES;
    var DETAIL_KEYS = MessageKeyConstants.DETAIL;
    var VIEW_KEYS = MessageKeyConstants.VIEW;
    var LIFECYCLE_TEXT_KEYS = {};
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.CANCELLED] = DETAIL_KEYS.STATUS_CANCELLED;
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.COMPLETED] = DETAIL_KEYS.STATUS_COMPLETED;
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.DRAFT] = DETAIL_KEYS.STATUS_DRAFT;
    LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.IN_PROCESS] = DETAIL_KEYS.STATUS_IN_PROCESS;
    var LIFECYCLE_STATES = {};
    LIFECYCLE_STATES[CHECKLIST_STATUSES.CANCELLED] = UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
    LIFECYCLE_STATES[CHECKLIST_STATUSES.COMPLETED] = UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
    LIFECYCLE_STATES[CHECKLIST_STATUSES.DRAFT] = UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    LIFECYCLE_STATES[CHECKLIST_STATUSES.IN_PROCESS] = UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
    var LOCK_STATE_SEMANTICS = {
        SUCCESS: UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS,
        ERROR: UiSemanticConstants.OBJECT_STATUS_STATE.ERROR
    };
    var LOCK_ACTIVE_STATES = {};
    var LOCK_TRANSITION_STATES = {};
    var AUTOSAVE_TEXT_KEYS = {};
    AUTOSAVE_TEXT_KEYS[WorkflowContracts.AUTOSAVE_STATES.IDLE] = VIEW_KEYS.AUTOSAVE_WAITING;
    AUTOSAVE_TEXT_KEYS[WorkflowContracts.AUTOSAVE_STATES.SAVING] = VIEW_KEYS.AUTOSAVE_SAVING;
    AUTOSAVE_TEXT_KEYS[WorkflowContracts.AUTOSAVE_STATES.SAVED] = VIEW_KEYS.AUTOSAVE_SAVED;
    AUTOSAVE_TEXT_KEYS[WorkflowContracts.AUTOSAVE_STATES.FAILED] = VIEW_KEYS.AUTOSAVE_ERROR;
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
    var oDateFormatter = DateFormat.getDateInstance({
        pattern: "dd.MM.yyyy"
    });
    var oTimeFormatter = DateFormat.getTimeInstance({
        pattern: "HH:mm"
    });
    LOCK_ACTIVE_STATES[WorkflowContracts.LOCK_STATES.EDIT_LOCKED] = true;
    LOCK_TRANSITION_STATES[WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK] = true;
    LOCK_TRANSITION_STATES[WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE] = true;

    function text(oController, sKey, vArgs) {
        var aArgs = [];
        var sResolvedText = "";
        if (Array.isArray(vArgs)) {
            aArgs = vArgs;
        }
            sResolvedText = ControllerTextRuntime.getText(oController, sKey, aArgs, "");
        return sResolvedText === sKey ? "" : sResolvedText;
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
        return oDateFormatter.format(oDate);
    }

    function formatDateTimeCardValue(oController, sDate, sTime, sTimezone) {
        var sHumanDate = formatHumanDate(sDate) || "-";
        var sTimeValue = String(sTime || "-");
        var sTimezoneValue = String(sTimezone || "-");
        return [
            text(oController, "dateLabel") + ": " + sHumanDate,
            text(oController, "timeLabel") + ": " + sTimeValue,
            text(oController, "timezoneLabel") + ": " + sTimezoneValue
        ].join("\n");
    }

    function formatAutosaveTime(vValue) {
        var oDate = parseDateLike(vValue);
        if (!oDate) {
            return vValue || "-";
        }
        return oTimeFormatter.format(oDate);
    }

    return {
        formatValidationState: function (bShown, bMissing) {
            return (bShown && bMissing) ? UiSemanticConstants.VALUE_STATE.ERROR : UiSemanticConstants.VALUE_STATE.NONE;
        },

        formatValidationText: function (bShown, bMissing) {
            return (bShown && bMissing) ? text(this, DETAIL_KEYS.REQUIRED_FIELD_HINT) : "";
        },

        formatWarningMessageType: function () {
            return UiSemanticConstants.MESSAGE_TYPE.WARNING;
        },

        formatValidationSummaryText: function (oSummary) {
            var iCount = readMissingCount(oSummary);
            if (iCount > 0) {
                return text(this, VIEW_KEYS.VALIDATION_SUMMARY_TITLE_COUNT, [iCount]);
            }
            return text(this, DETAIL_KEYS.VALIDATION_SUMMARY_TITLE);
        },

        formatValidationSummaryLinkText: function (oSummary) {
            var iCount = readMissingCount(oSummary);
            if (iCount > 1) {
                return text(this, VIEW_KEYS.VALIDATION_SUMMARY_LINK_MANY, [iCount]);
            }
            if (iCount === 1) {
                return text(this, VIEW_KEYS.VALIDATION_SUMMARY_LINK_SINGLE);
            }
            return text(this, DETAIL_KEYS.REQUIRED_FIELD_HINT);
        },

        formatBooleanResultText: function (vValue) {
            if (typeof vValue === "string") {
                var sValue = String(vValue).trim().toUpperCase();
                if (sValue === "OK" || sValue === "TRUE" || sValue === "X") {
                    return text(this, DETAIL_KEYS.STATUS_OK);
                }
                if (sValue === "FAILED" || sValue === "ERROR" || sValue === "FALSE") {
                    return text(this, DETAIL_KEYS.STATUS_FAILED);
                }
                return "-";
            }
            if (vValue === true) {
                return text(this, DETAIL_KEYS.STATUS_OK);
            }
            if (vValue === false) {
                return text(this, DETAIL_KEYS.STATUS_FAILED);
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
            return text(this, LIFECYCLE_TEXT_KEYS[sNormalized] || LIFECYCLE_TEXT_KEYS[CHECKLIST_STATUSES.DRAFT]);
        },

        formatLifecycleStatusState: function (sStatus) {
            var sNormalized = String(sStatus || "").toUpperCase();
            return LIFECYCLE_STATES[sNormalized] || LIFECYCLE_STATES[CHECKLIST_STATUSES.DRAFT];
        },

        formatDraftStateText: function (bDirty, bCreateMode) {
            if (bDirty) {
                return text(this, DETAIL_KEYS.DETAIL_DRAFT_CHANGED);
            }
            if (bCreateMode) {
                return text(this, DETAIL_KEYS.DETAIL_DRAFT_LOCAL);
            }
            return text(this, DETAIL_KEYS.DETAIL_DRAFT_CLEAN);
        },

        formatDraftStateState: function (bDirty, bCreateMode) {
            if (bDirty || bCreateMode) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
            }
            return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        },

        formatLockStateText: function (sTextValue, sMode) {
            if (sTextValue) {
                return sTextValue;
            }
            return WorkflowContracts.isEditableMode(sMode)
                ? text(this, VIEW_KEYS.MODE_EDIT)
                : text(this, VIEW_KEYS.MODE_READ);
        },

        formatLockStateSemantic: function (sState, sMode) {
            var sNormalizedState = String(sState || "").toUpperCase();
            if (LOCK_STATE_SEMANTICS[sNormalizedState]) {
                return LOCK_STATE_SEMANTICS[sNormalizedState];
            }
            if (LOCK_ACTIVE_STATES[sNormalizedState]) {
                return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
            }
            if (sNormalizedState === WorkflowContracts.LOCK_STATES.LOCK_LOST || sNormalizedState === WorkflowContracts.LOCK_STATES.FORCED_READ_ONLY) {
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
                return text(this, VIEW_KEYS.HEARTBEAT_LOCKED_ACTIVE);
            }
            if (WorkflowContracts.normalizeEditMode(sMode) === WorkflowContracts.EDIT_MODES.CREATE) {
                return text(this, VIEW_KEYS.HEARTBEAT_DRAFT_CREATE);
            }
            if (sNormalizedLockState === WorkflowContracts.LOCK_STATES.ACQUIRING_LOCK) {
                return text(this, VIEW_KEYS.AUTOSAVE_SAVING);
            }
            if (sNormalizedLockState === WorkflowContracts.LOCK_STATES.IDLE_TIMEOUT_GRACE) {
                return text(this, VIEW_KEYS.IDLE_TIMEOUT_GRACE_BANNER_TITLE);
            }
            if (sNormalizedLockState === WorkflowContracts.LOCK_STATES.LOCK_LOST) {
                return text(this, DETAIL_KEYS.LOCK_LOST_BANNER_TITLE);
            }
            if (sNormalizedLockState === WorkflowContracts.LOCK_STATES.FORCED_READ_ONLY) {
                return text(this, DETAIL_KEYS.DETAIL_FORCED_READ_ONLY_TITLE);
            }
            return text(this, VIEW_KEYS.HEARTBEAT_INACTIVE);
        },

        formatAutosaveText: function (sMode, sLockState, sAutosaveState) {
            var sState = WorkflowContracts.normalizeAutosaveState(sAutosaveState);
            var sNormalizedMode = WorkflowContracts.normalizeEditMode(sMode);
            var sNormalizedLockState = String(sLockState || "").toUpperCase();
            if (sNormalizedMode === WorkflowContracts.EDIT_MODES.CREATE) {
                return text(this, VIEW_KEYS.AUTOSAVE_CREATE_DRAFT);
            }
            if (sNormalizedMode !== WorkflowContracts.EDIT_MODES.EDIT || !LOCK_ACTIVE_STATES[sNormalizedLockState]) {
                return text(this, VIEW_KEYS.AUTOSAVE_DISABLED);
            }
            return text(this, AUTOSAVE_TEXT_KEYS[sState] || VIEW_KEYS.AUTOSAVE_WAITING);
        },

        formatPersistenceText: function (sMessageKey, sPersistenceState, sLastSavedAt, bCreateMode) {
            if (bCreateMode && sPersistenceState !== "saved") {
                return text(this, VIEW_KEYS.AUTOSAVE_CREATE_DRAFT);
            }
            if (sMessageKey) {
                if (sMessageKey === VIEW_KEYS.PERSISTENCE_SAVED || sMessageKey === VIEW_KEYS.PERSISTENCE_AUTOSAVE_SAVED) {
                    if (sLastSavedAt) {
                        return text(this, VIEW_KEYS.PERSISTENCE_SAVED_AT, [formatAutosaveTime(sLastSavedAt)]);
                    }
                }
                return text(this, sMessageKey);
            }
            if (sPersistenceState === "saved" && sLastSavedAt) {
                return text(this, VIEW_KEYS.PERSISTENCE_SAVED_AT, [formatAutosaveTime(sLastSavedAt)]);
            }
            return text(this, VIEW_KEYS.PERSISTENCE_IDLE);
        },

        formatPersistenceTooltip: function (sMessageKey, sPersistenceState, sLastSavedAt, oLastSaveError) {
            var sPrimary = this.formatPersistenceText(sMessageKey, sPersistenceState, sLastSavedAt);
            var sCode = String((oLastSaveError && oLastSaveError.code) || "").trim();
            var sMessage = String((oLastSaveError && oLastSaveError.message) || "").trim();
            if (!sCode && !sMessage) {
                return sPrimary;
            }
            return text(this, VIEW_KEYS.PERSISTENCE_TOOLTIP_WITH_DETAILS, [sPrimary, [sCode, sMessage].filter(Boolean).join(": ")]);
        },

        formatPersistenceState: function (sPersistenceState) {
            return PERSISTENCE_SEMANTICS[String(sPersistenceState || "").trim()] || UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
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
                datetime: formatDateTimeCardValue(this, sDate, sTime, sTimezone),
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

        formatPersistedActionVisible: function (sActiveObjectId) {
            return !!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId);
        },

        formatEditActionVisible: function (sMode) {
            return WorkflowContracts.isEditableMode(sMode);
        },

        formatEditSwitchState: function (sMode) {
            return WorkflowContracts.isEditableMode(sMode);
        },

        formatEditSwitchEnabled: function (sActiveObjectId, bLockOperationPending) {
            return !!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId) && !bLockOperationPending;
        },

        formatDeleteChecklistVisible: function (sMode, sActiveObjectId) {
            return WorkflowContracts.isEditableMode(sMode) &&
                !!sActiveObjectId &&
                !CreateSentinel.isCreateId(sActiveObjectId);
        },

        formatDeleteChecklistConfirmVisible: function (bArmed, sMode, sActiveObjectId) {
            return !!bArmed && this.formatDeleteChecklistVisible(sMode, sActiveObjectId);
        },

        formatAttachmentsEmptyStateText: function (sRootId) {
            return CreateSentinel.isCreateId(sRootId)
                ? text(this, DETAIL_KEYS.ATTACHMENT_DRAFT_STAGE_HINT)
                : text(this, DETAIL_KEYS.DETAIL_EMPTY_ATTACHMENTS_SAVED_TEXT) || text(this, DETAIL_KEYS.DETAIL_EMPTY_ATTACHMENTS_TEXT);
        },

        formatIntegrationVisible: function (vFlag) {
            return !!vFlag;
        },

        formatIntegrationText: function (vFlag) {
            return vFlag ? text(this, DETAIL_KEYS.INTEGRATION_SOURCE_BADGE) : "";
        }
    };
});
