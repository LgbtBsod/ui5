sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (WorkflowContracts, DetailPersistenceRuntime) {
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
        persistence: DetailPersistenceRuntime.createInitialPersistenceState(),
        validationSummary: {
            hasErrors: false,
            missingPaths: [],
            missingKeys: [],
            source: WorkflowContracts.VALIDATION_STATUS.IDLE,
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
        capabilityStatus: WorkflowContracts.READINESS_STATUS.PENDING,
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
