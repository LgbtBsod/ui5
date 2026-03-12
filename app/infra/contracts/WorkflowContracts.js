/* Infra-local workflow contract mirror to keep infra independent from service/usecase imports. */
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
        if (Object.keys(LOCK_STATES).some(function (sKey) { return LOCK_STATES[sKey] === sState; })) {
            return sState;
        }
        return LOCK_STATES.READ_ONLY;
    }

    function isEditLocked(vMode, vLockState) {
        return normalizeEditMode(vMode) === EDIT_MODES.EDIT && normalizeLockState(vLockState) === LOCK_STATES.EDIT_LOCKED;
    }

    return Object.freeze({
        INFRA_CONTRACT_SCOPE: "infra",
        EDIT_MODES: EDIT_MODES,
        LOCK_STATES: LOCK_STATES,
        normalizeEditMode: normalizeEditMode,
        normalizeLockState: normalizeLockState,
        isEditLocked: isEditLocked
    });
});
