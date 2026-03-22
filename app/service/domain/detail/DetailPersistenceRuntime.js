sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Effects, StatePaths, WorkflowContracts, DetailContracts, ModelContracts) {
    "use strict";

    var STATES = DetailContracts.STATES;
    var TAXONOMY = DetailContracts.TAXONOMY;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_MESSAGE_KEYS = DetailContracts;

    function upper(vValue) {
        return String(vValue || "").trim().toUpperCase();
    }

    function firstNonEmpty() {
        var iIndex;
        for (iIndex = 0; iIndex < arguments.length; iIndex += 1) {
            if (String(arguments[iIndex] || "").trim()) {
                return String(arguments[iIndex] || "").trim();
            }
        }
        return "";
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
        var sBackendCode = upper(firstNonEmpty(
            oError && oError.backend && oError.backend.code,
            oError && oError.code,
            oError && oError.kind
        ));
        if (sBackendCode === TAXONOMY.LOCK_EXPIRED) {
            return {
                taxonomy: TAXONOMY.LOCK_EXPIRED,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_EXPIRED
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_STOLEN) {
            return {
                taxonomy: TAXONOMY.LOCK_STOLEN,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_STOLEN
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_NOT_OWNED_BY_SESSION) {
            return {
                taxonomy: TAXONOMY.LOCK_NOT_OWNED_BY_SESSION,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_NOT_OWNED
            };
        }
        if (sBackendCode === TAXONOMY.LOCK_MISSING) {
            return {
                taxonomy: TAXONOMY.LOCK_MISSING,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_MISSING
            };
        }
        if (iStatus === 401 || iStatus === 403 || hasAny(sSignal, ["PERMISSION", "AUTH", "SESSION_EXPIRED"])) {
            return {
                taxonomy: TAXONOMY.PERMISSION_DENIED,
                persistenceState: STATES.ERROR,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_PERMISSION_DENIED
            };
        }
        if (iStatus === 409 || hasAny(sSignal, ["ETAG", "VERSION_CONFLICT", "CONFLICT", "STALE"])) {
            return {
                taxonomy: TAXONOMY.VERSION_CONFLICT,
                persistenceState: STATES.CONFLICT,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_CONFLICT
            };
        }
        if (hasAny(sSignal, ["LOCK_STOLEN", "LOCK_REPLACED", "KILLED", "TAKEN OVER", "TAKEOVER"])) {
            return {
                taxonomy: TAXONOMY.LOCK_STOLEN,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_STOLEN
            };
        }
        if (hasAny(sSignal, ["LOCK_EXPIRED", "EXPIRED"])) {
            return {
                taxonomy: TAXONOMY.LOCK_EXPIRED,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_EXPIRED
            };
        }
        if (hasAny(sSignal, ["NOT_OWNED", "OWNER_SESSION", "OWNED_BY_OTHER", "OWNED BY ANOTHER", "SESSION"])) {
            return {
                taxonomy: TAXONOMY.LOCK_NOT_OWNED_BY_SESSION,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_NOT_OWNED
            };
        }
        if (hasAny(sSignal, ["LOCK_MISSING", "LOCK_REQUIRED", "MISSING LOCK", "NOT_FOUND"])) {
            return {
                taxonomy: TAXONOMY.LOCK_MISSING,
                persistenceState: STATES.LOCK_LOST,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_LOCK_MISSING
            };
        }
        if (hasAny(sSignal, ["VALIDATION"])) {
            return {
                taxonomy: TAXONOMY.VALIDATION_ERROR,
                persistenceState: STATES.ERROR,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_VALIDATION_ERROR
            };
        }
        if (iStatus === 0 || iStatus === 408 || hasAny(sSignal, ["NETWORK", "TIMEOUT", "OFFLINE"])) {
            return {
                taxonomy: TAXONOMY.NETWORK_ERROR,
                persistenceState: STATES.ERROR,
                messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_NETWORK_ERROR
            };
        }
        return {
            taxonomy: TAXONOMY.TECHNICAL_ERROR,
            persistenceState: STATES.ERROR,
            messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_TECHNICAL_ERROR
        };
    }

    function buildWriteId(sPrefix) {
        return [sPrefix || "write", Date.now(), Math.round(Math.random() * 100000)].join("-");
    }

    function createPayload(mPartial) {
        return Object.assign({
            state: STATES.IDLE,
            messageKey: DETAIL_MESSAGE_KEYS.PERSISTENCE_IDLE,
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
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE, oPayload),
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, (function () {
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
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, oPayload.lastSavedAt),
            Effects.modelPatch(STATE_MODEL, StatePaths.SAVE_IN_FLIGHT, !!(oPayload.isManualSaveInFlight || oPayload.isAutoSaveInFlight))
        ];
    }

    function startEffects(sKind, sRequestId, mExtra) {
        return modelEffects(Object.assign({
            state: sKind === "manual" ? STATES.SAVING : STATES.AUTOSAVING,
            messageKey: sKind === "manual" ? DETAIL_MESSAGE_KEYS.PERSISTENCE_SAVING : DETAIL_MESSAGE_KEYS.PERSISTENCE_AUTOSAVING,
            currentWriteRequestId: sRequestId || buildWriteId(sKind),
            isManualSaveInFlight: sKind === "manual",
            isAutoSaveInFlight: sKind === "auto"
        }, mExtra || {}));
    }

    function successEffects(sKind, sSavedAt, mExtra) {
        return modelEffects(Object.assign({
            state: STATES.SAVED,
            messageKey: sKind === "manual" ? DETAIL_MESSAGE_KEYS.PERSISTENCE_SAVED : DETAIL_MESSAGE_KEYS.PERSISTENCE_AUTOSAVE_SAVED,
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
            messageKey: bDirty ? DETAIL_MESSAGE_KEYS.PERSISTENCE_DIRTY : DETAIL_MESSAGE_KEYS.PERSISTENCE_IDLE
        }, mExtra || {}));
    }

    function canAutosaveFromState(oUiState, mOptions) {
        var STATE_MODEL_NAME = (mOptions && mOptions.stateModelName) || STATE_MODEL;
        var sRootId = String((mOptions && mOptions.rootId) || "").trim();
        var bIsCreateDraft = !!(mOptions && mOptions.isCreateDraft);
        var sEditMode = WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        var sLockState = WorkflowContracts.normalizeLockState(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.WORKFLOW_DETAIL_LOCK_STATE));
        var bDirty = !!(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.WORKFLOW_DIRTY));
        var bHasValidLock = !!(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.PERSISTENCE_HAS_VALID_LOCK));
        var bLockOwnerSessionMatches = !!(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES));
        var bHasConflict = !!(oUiState && oUiState.get(STATE_MODEL_NAME, "/hasConflict"));

        return WorkflowContracts.isEditLocked(sEditMode, sLockState) &&
            bDirty &&
            bHasValidLock &&
            bLockOwnerSessionMatches &&
            !bHasConflict &&
            !!sRootId &&
            !bIsCreateDraft;
    }

    function canScheduleAutosave(oUiState, mOptions) {
        var STATE_MODEL_NAME = (mOptions && mOptions.stateModelName) || STATE_MODEL;
        return canAutosaveFromState(oUiState, mOptions) &&
            !(oUiState && oUiState.get(STATE_MODEL_NAME, StatePaths.SAVE_IN_FLIGHT));
    }

    return {
        STATES: STATES,
        TAXONOMY: TAXONOMY,
        buildWriteId: buildWriteId,
        canAutosaveFromState: canAutosaveFromState,
        canScheduleAutosave: canScheduleAutosave,
        classifyError: classifyError,
        dirtyEffects: dirtyEffects,
        extractSignal: extractSignal,
        failureEffects: failureEffects,
        isLockFailure: isLockFailure,
        startEffects: startEffects,
        successEffects: successEffects
    };
});
