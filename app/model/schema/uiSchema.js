sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (WorkflowContracts, WorkflowRuntimeConstants, DetailMessageKeyConstants) {
    "use strict";

    return {
        isLoading: false,
        loadError: false,
        loadErrorMessage: "",
        saveInFlight: false,
        pendingNavigationIntent: null,
        isDirty: false,
        isKilled: false,
        hasConflict: false,
        lockExpires: null,
        cacheValidationAt: "",
        idleExpires: null,
        lockOperationPending: false,
    lockStateText: "",
        lockLostReason: "",
        networkOnline: true,
        networkGraceMode: false,
        networkGraceExpiresAt: null,
        autosaveState: WorkflowContracts.AUTOSAVE_STATES.IDLE,
        autosaveAt: null,
        autosaveEnabled: false,
        persistence: {
            state: WorkflowRuntimeConstants.PERSISTENCE_STATES.IDLE,
            messageKey: DetailMessageKeyConstants.PERSISTENCE_IDLE,
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
        },
        validationSummary: {
            hasErrors: false,
            missingPaths: [],
            missingKeys: [],
            source: WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE,
            firstMissingPath: "",
            firstMissingKey: ""
        },
        tabConflictState: {
            active: false,
            source: "",
            at: ""
        },
        postOpenHydratedRootId: "",
        requiredFields: [],
        mainServiceMetadataOk: null,
        mainServiceMetadataError: "",
        capabilityStatus: WorkflowRuntimeConstants.READINESS_STATUS.PENDING,
        capabilityDegradedReason: "",
        capabilityMessageKey: "capabilityPending",
        capabilityDiagnostics: {},
        masterDataLoading: false,
        locationsLoading: false,
        telemetry: {
            events: [],
            lastEvent: null
        },
        ui: {
            busy: { global: false, searchTable: false, detail: false, analytics: false, export: false },
            feedback: {
                banner: {
                    global: {
                    visible: false,
                    severity: "info",
                        text: "",
                        details: "",
                        correlationId: "",
                        retryAction: "",
                        retryTextKey: ""
                    }
                }
            }
        }
    };
});
