sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (Effects, StatePaths, WorkflowContracts) {
    "use strict";

    var STATES = Object.freeze({
        IDLE: "idle",
        DIRTY: "dirty",
        SAVING: "saving",
        AUTOSAVING: "autosaving",
        SAVED: "saved",
        ERROR: "error",
        LOCK_LOST: "lockLost",
        READ_ONLY: "readOnly",
        IDLE_TIMEOUT_GRACE: "idleTimeoutGrace",
        CONFLICT: "conflict"
    });

    var TAXONOMY = Object.freeze({
        PERMISSION_DENIED: "PERMISSION_DENIED",
        LOCK_EXPIRED: "LOCK_EXPIRED",
        LOCK_NOT_OWNED_BY_SESSION: "LOCK_NOT_OWNED_BY_SESSION",
        LOCK_STOLEN: "LOCK_STOLEN",
        LOCK_MISSING: "LOCK_MISSING",
        VERSION_CONFLICT: "VERSION_CONFLICT",
        VALIDATION_ERROR: "VALIDATION_ERROR",
        NETWORK_ERROR: "NETWORK_ERROR",
        TECHNICAL_ERROR: "TECHNICAL_ERROR"
    });

    function upper(vValue) {
        return String(vValue || "").trim().toUpperCase();
    }

    function extractSignal(oError) {
        var aSignals = [];
        var aDetails = Array.isArray(oError && oError.details) ? oError.details : [];
        var oBackend = oError && oError.backend;
        aSignals.push(upper(oError && oError.code));
        aSignals.push(upper(oError && oError.kind));
        aSignals.push(upper(oError && oError.message));
        aSignals.push(upper(oBackend && oBackend.code));
        aDetails.forEach(function (oDetail) {
            aSignals.push(upper(oDetail && oDetail.code));
            aSignals.push(upper(oDetail && oDetail.message));
        });
        return aSignals.join(" ");
    }

    function hasAny(sSignal, aNeedles) {
        return aNeedles.some(function (sNeedle) {
            return sSignal.indexOf(String(sNeedle || "").toUpperCase()) >= 0;
        });
    }

    function isLockFailure(sTaxonomy) {
        return sTaxonomy === TAXONOMY.LOCK_EXPIRED ||
            sTaxonomy === TAXONOMY.LOCK_NOT_OWNED_BY_SESSION ||
            sTaxonomy === TAXONOMY.LOCK_STOLEN ||
            sTaxonomy === TAXONOMY.LOCK_MISSING;
    }

    function classifyError(oError) {
        var sSignal = extractSignal(oError);
        var iStatus = Number((oError && (oError.statusCode || oError.status)) || 0) || 0;
        var sBackendCode = upper(oError && oError.backend && oError.backend.code);
        if (sBackendCode === TAXONOMY.LOCK_EXPIRED) {
            return {
                taxonomy: TAXONOMY.LOCK_EXPIRED,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockExpired"
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_STOLEN) {
            return {
                taxonomy: TAXONOMY.LOCK_STOLEN,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockStolen"
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_NOT_OWNED_BY_SESSION) {
            return {
                taxonomy: TAXONOMY.LOCK_NOT_OWNED_BY_SESSION,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockNotOwned"
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_MISSING) {
            return {
                taxonomy: TAXONOMY.LOCK_MISSING,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockMissing"
            };
        }
        if (iStatus === 401 || iStatus === 403 || hasAny(sSignal, ["PERMISSION", "AUTH", "SESSION_EXPIRED"])) {
            return {
                taxonomy: TAXONOMY.PERMISSION_DENIED,
                persistenceState: STATES.ERROR,
                messageKey: "persistencePermissionDenied"
            };
        }
        if (iStatus === 409 || hasAny(sSignal, ["ETAG", "VERSION_CONFLICT", "CONFLICT", "STALE"])) {
            return {
                taxonomy: TAXONOMY.VERSION_CONFLICT,
                persistenceState: STATES.CONFLICT,
                messageKey: "persistenceConflict"
            };
        }
        if (hasAny(sSignal, ["LOCK_STOLEN", "LOCK_REPLACED", "KILLED", "TAKEN OVER", "TAKEOVER"])) {
            return {
                taxonomy: TAXONOMY.LOCK_STOLEN,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockStolen"
            };
        }
        if (hasAny(sSignal, ["LOCK_EXPIRED", "EXPIRED"])) {
            return {
                taxonomy: TAXONOMY.LOCK_EXPIRED,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockExpired"
            };
        }
        if (hasAny(sSignal, ["NOT_OWNED", "OWNER_SESSION", "OWNED_BY_OTHER", "OWNED BY ANOTHER", "SESSION"])) {
            return {
                taxonomy: TAXONOMY.LOCK_NOT_OWNED_BY_SESSION,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockNotOwned"
            };
        }
        if (hasAny(sSignal, ["LOCK_MISSING", "LOCK_REQUIRED", "MISSING LOCK", "NOT_FOUND"])) {
            return {
                taxonomy: TAXONOMY.LOCK_MISSING,
                persistenceState: STATES.LOCK_LOST,
                messageKey: "persistenceLockMissing"
            };
        }
        if (hasAny(sSignal, ["VALIDATION"])) {
            return {
                taxonomy: TAXONOMY.VALIDATION_ERROR,
                persistenceState: STATES.ERROR,
                messageKey: "persistenceValidationError"
            };
        }
        if (iStatus === 0 || iStatus === 408 || hasAny(sSignal, ["NETWORK", "TIMEOUT", "OFFLINE"])) {
            return {
                taxonomy: TAXONOMY.NETWORK_ERROR,
                persistenceState: STATES.ERROR,
                messageKey: "persistenceNetworkError"
            };
        }
        return {
            taxonomy: TAXONOMY.TECHNICAL_ERROR,
            persistenceState: STATES.ERROR,
            messageKey: "persistenceTechnicalError"
        };
    }

    function buildWriteId(sPrefix) {
        return [sPrefix || "write", Date.now(), Math.round(Math.random() * 100000)].join("-");
    }

    function createPayload(mPartial) {
        return Object.assign({
            state: STATES.IDLE,
            messageKey: "persistenceIdle",
            lastSavedAt: null,
            lastSaveError: null,
            taxonomy: "",
            currentWriteRequestId: "",
            isManualSaveInFlight: false,
            isAutoSaveInFlight: false,
            hasValidLock: false,
            lockOwnerSessionMatches: false,
            lastLockRefreshAt: null,
            nextHeartbeatAt: null
        }, mPartial || {});
    }

    function modelEffects(mPayload) {
        var oPayload = createPayload(mPayload);
        return [
            Effects.modelPatch("state", "/persistence", oPayload),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, (function () {
                if (oPayload.state === STATES.SAVING || oPayload.state === STATES.AUTOSAVING) {
                    return WorkflowContracts.AUTOSAVE_STATES.SAVING;
                }
                if (oPayload.state === STATES.SAVED) {
                    return WorkflowContracts.AUTOSAVE_STATES.SAVED;
                }
                if (oPayload.state === STATES.ERROR || oPayload.state === STATES.LOCK_LOST || oPayload.state === STATES.CONFLICT || oPayload.state === STATES.READ_ONLY || oPayload.state === STATES.IDLE_TIMEOUT_GRACE) {
                    return WorkflowContracts.AUTOSAVE_STATES.FAILED;
                }
                return WorkflowContracts.AUTOSAVE_STATES.IDLE;
            }())),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, oPayload.lastSavedAt),
            Effects.modelPatch("state", StatePaths.SAVE_IN_FLIGHT, !!(oPayload.isManualSaveInFlight || oPayload.isAutoSaveInFlight))
        ];
    }

    function startEffects(sKind, sRequestId, mExtra) {
        return modelEffects(Object.assign({
            state: sKind === "manual" ? STATES.SAVING : STATES.AUTOSAVING,
            messageKey: sKind === "manual" ? "persistenceSaving" : "persistenceAutosaving",
            currentWriteRequestId: sRequestId || buildWriteId(sKind),
            isManualSaveInFlight: sKind === "manual",
            isAutoSaveInFlight: sKind === "auto"
        }, mExtra || {}));
    }

    function successEffects(sKind, sSavedAt, mExtra) {
        return modelEffects(Object.assign({
            state: STATES.SAVED,
            messageKey: sKind === "manual" ? "persistenceSaved" : "persistenceAutosaveSaved",
            lastSavedAt: sSavedAt || new Date().toISOString(),
            lastSaveError: null,
            taxonomy: "",
            currentWriteRequestId: "",
            isManualSaveInFlight: false,
            isAutoSaveInFlight: false
        }, mExtra || {}));
    }

    function failureEffects(sKind, oError, mExtra) {
        var oClassified = classifyError(oError);
        var oPayload = Object.assign({
            state: oClassified.persistenceState,
            messageKey: oClassified.messageKey,
            lastSaveError: {
                code: String((oError && oError.code) || ""),
                message: String((oError && oError.message) || ""),
                taxonomy: oClassified.taxonomy
            },
            taxonomy: oClassified.taxonomy,
            currentWriteRequestId: "",
            isManualSaveInFlight: false,
            isAutoSaveInFlight: false
        }, mExtra || {});
        if (isLockFailure(oClassified.taxonomy)) {
            oPayload.hasValidLock = false;
            oPayload.lockOwnerSessionMatches = false;
        }
        return {
            classification: oClassified,
            effects: modelEffects(oPayload)
        };
    }

    function dirtyEffects(bDirty, mExtra) {
        return modelEffects(Object.assign({
            state: bDirty ? STATES.DIRTY : STATES.IDLE,
            messageKey: bDirty ? "persistenceDirty" : "persistenceIdle"
        }, mExtra || {}));
    }

    return {
        STATES: STATES,
        TAXONOMY: TAXONOMY,
        buildWriteId: buildWriteId,
        classifyError: classifyError,
        dirtyEffects: dirtyEffects,
        failureEffects: failureEffects,
        isLockFailure: isLockFailure,
        startEffects: startEffects,
        successEffects: successEffects
    };
});
