sap.ui.define([], function () {
    "use strict";

    return {
        isLoading: false,
        loadError: false,
        loadErrorMessage: "",
        isBusy: false,
        saveInFlight: false,
        pendingNavigationIntent: null,
        isDirty: false,
        isKilled: false,
        hasConflict: false,
        lockExpires: null,
        cacheValidationAt: "",
        idleExpires: null,
        lockOperationPending: false,
        lockOperationState: "IDLE",
        lockOperationText: "",
        lockLostReason: "",
        networkOnline: true,
        networkGraceMode: false,
        networkGraceExpiresAt: null,
        autosaveState: "IDLE",
        autosaveAt: null,
        autosaveEnabled: false,
        validationSummary: {
            hasErrors: false,
            missingPaths: [],
            missingKeys: [],
            source: "idle",
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
        capabilityStatus: "pending",
        capabilityDegradedReason: "",
        capabilityMessageKey: "capabilityPending",
        capabilityDiagnostics: {},
        masterDataLoading: false,
        locationsLoading: false,
        ui: {
            busy: { global: false, searchTable: false, detail: false },
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
