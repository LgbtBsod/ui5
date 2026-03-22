sap.ui.define([], function () {
    "use strict";

    var EDIT_MODES = Object.freeze({
        READ: "READ",
        EDIT: "EDIT",
        CREATE: "CREATE"
    });

    var LOCK_STATES = Object.freeze({
        IDLE: "IDLE",
        READ_ONLY: "READ_ONLY",
        ACQUIRING_LOCK: "ACQUIRING_LOCK",
        EDIT_LOCKED: "EDIT_LOCKED",
        LOCK_LOST: "LOCK_LOST",
        IDLE_TIMEOUT_GRACE: "IDLE_TIMEOUT_GRACE",
        FORCED_READ_ONLY: "FORCED_READ_ONLY"
    });

    var AUTOSAVE_STATES = Object.freeze({
        IDLE: "IDLE",
        SAVING: "SAVING",
        SAVED: "SAVED",
        FAILED: "FAILED"
    });

    var CHECKLIST_STATUSES = Object.freeze({
        CANCELLED: "CANCELLED",
        COMPLETED: "COMPLETED",
        DRAFT: "DRAFT",
        IN_PROCESS: "IN_PROCESS"
    });

    var REASONS = Object.freeze({
        FREE: "FREE",
        READ_ONLY: "READ_ONLY",
        IDLE_TIMEOUT: "IDLE_TIMEOUT",
        KILLED: "KILLED",
        EXPIRED: "EXPIRED",
        LOCK_EXPIRED: "LOCK_EXPIRED",
        LOST: "LOST",
        OWNED_BY_YOU: "OWNED_BY_YOU"
    });

    var REQUEST_GROUPS = Object.freeze({
        AUTOSAVE: "autosave",
        CHANGES: "changes",
        LOCKS: "locks",
        SAVE_FLOW: "saveFlow"
    });

    var READINESS_STATUS = Object.freeze({
        DENIED: "denied",
        ERROR: "error",
        IDLE: "idle",
        LOADING: "loading",
        PENDING: "pending",
        READY: "ready"
    });

    var PERSISTENCE_STATES = Object.freeze({
        AUTOSAVING: "autosaving",
        CONFLICT: "conflict",
        DIRTY: "dirty",
        ERROR: "error",
        IDLE: "idle",
        IDLE_TIMEOUT_GRACE: "idleTimeoutGrace",
        LOCK_LOST: "lockLost",
        READ_ONLY: "readOnly",
        SAVED: "saved",
        SAVING: "saving"
    });

    var VALIDATION_STATUS = Object.freeze({
        IDLE: "idle"
    });

    var SOURCES = Object.freeze({
        AUTOSAVE: "autosave",
        ENTER_EDIT: "enter_edit",
        GATEWAY: "gateway",
        IDLE: "idle"
    });

    function hasValue(oMap, sValue) {
        return Object.keys(oMap).some(function (sKey) {
            return oMap[sKey] === sValue;
        });
    }

    function normalizeEditMode(vMode) {
        var sMode = String(vMode || "").trim().toUpperCase();
        if (sMode === EDIT_MODES.EDIT || sMode === EDIT_MODES.CREATE) {
            return sMode;
        }
        return EDIT_MODES.READ;
    }

    function normalizeLockState(vState) {
        var sState = String(vState || "").trim().toUpperCase();
        if (!sState) {
            return LOCK_STATES.READ_ONLY;
        }
        if (hasValue(LOCK_STATES, sState)) {
            return sState;
        }
        return LOCK_STATES.READ_ONLY;
    }

    function normalizeAutosaveState(vState) {
        var sState = String(vState || "").trim().toUpperCase();
        if (!sState) {
            return AUTOSAVE_STATES.IDLE;
        }
        if (hasValue(AUTOSAVE_STATES, sState)) {
            return sState;
        }
        return AUTOSAVE_STATES.IDLE;
    }

    function isEditableMode(vMode) {
        var sMode = normalizeEditMode(vMode);
        return sMode === EDIT_MODES.EDIT || sMode === EDIT_MODES.CREATE;
    }

    function isEditLocked(vMode, vLockState) {
        return normalizeEditMode(vMode) === EDIT_MODES.EDIT &&
            normalizeLockState(vLockState) === LOCK_STATES.EDIT_LOCKED;
    }

    return Object.freeze({
        EDIT_MODES: EDIT_MODES,
        LOCK_STATES: LOCK_STATES,
        AUTOSAVE_STATES: AUTOSAVE_STATES,
        CHECKLIST_STATUSES: CHECKLIST_STATUSES,
        REASONS: REASONS,
        REQUEST_GROUPS: REQUEST_GROUPS,
        READINESS_STATUS: READINESS_STATUS,
        PERSISTENCE_STATES: PERSISTENCE_STATES,
        VALIDATION_STATUS: VALIDATION_STATUS,
        SOURCES: SOURCES,
        normalizeEditMode: normalizeEditMode,
        normalizeLockState: normalizeLockState,
        normalizeAutosaveState: normalizeAutosaveState,
        isEditableMode: isEditableMode,
        isEditLocked: isEditLocked
    });
});
