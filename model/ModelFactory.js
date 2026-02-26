sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function (JSONModel) {
    "use strict";

    return {

        createDataModel: function () {
            return new JSONModel({
                checkLists: [],
                visibleCheckLists: [],
                selectedChecklist: null
            });
        },

        createStateModel: function () {
            return new JSONModel({
                mode: "READ",
                layout: "OneColumn",
                isLoading: false,
                loadError: false,
                loadErrorMessage: "",
                filterId: "",
                filterLpc: "",
                filterFailedChecks: "ALL",
                filterFailedBarriers: "ALL",
                searchMode: "EXACT",
                // Search cap requested by user; empty means load all available rows.
                searchMaxResults: "100",
                isBusy: false,
                isDirty: false,
                isLocked: false,
                isKilled: false,
                hasConflict: false,
                sessionId: null,
                activeObjectId: null,
                copySourceId: null,
                lockExpires: null,
                cacheValidationAt: "",
                idleExpires: null,
                requiresUserLogin: true,
                testUser: "",
                testUserLogin: "",
                lockOperationPending: false,
                lockOperationState: "IDLE",
                lockOperationText: "",
                navGuardBypass: false,
                networkOnline: true,
                networkGraceMode: false,
                networkGraceExpiresAt: null,
                autosaveState: "IDLE",
                autosaveAt: null,
                requiredFields: [],
                mainServiceMetadataOk: null,
                mainServiceMetadataError: "",
                capabilityStatus: "pending",
                capabilityDegradedReason: "",
                capabilityMessageKey: "capabilityPending",
                capabilityDiagnostics: {},
                timers: {
                    heartbeatMs: 240000,
                    lockStatusMs: 60000,
                    gcdMs: 300000,
                    idleMs: 600000,
                    autoSaveIntervalMs: 60000,
                    autoSaveDebounceMs: 30000,
                    networkGraceMs: 60000,
                    cacheFreshMs: 30000,
                    cacheStaleOkMs: 90000,
                    analyticsRefreshMs: 900000
                },
                operationalKpiSnapshots: [],
                operationalKpiSnapshotLimit: 50,
                masterDataLoading: false,
                locationsLoading: false,
                operationalKpi: {
                    saveAttempts: 0,
                    saveSuccess: 0,
                    saveFailed: 0,
                    saveLatencyMsLast: 0,
                    saveLatencyMsAvg: 0,
                    saveLatencySamples: 0,
                    conflictCount: 0,
                    validationFailures: 0,
                    retryFailures: 0,
                    retryLatencyMsLast: 0,
                    retryLatencyMsAvg: 0,
                    retryLatencySamples: 0
                }
            });
        },

        createLayoutModel: function () {
            return new JSONModel({
                smartFilter: {
                    useCustomSmartFilters: true,
                    variant: "default",
                    fields: []
                },
                smartTable: {
                    useCustomSmartTable: true,
                    variant: "default",
                    columns: [],
                    selectionMode: "SingleSelectMaster"
                },
                personalization: {
                    compactRows: false,
                    showHints: true
                }
            });
        },

        createCacheModel: function () {
            return new JSONModel({
                pristineSnapshot: null,
                keyMapping: {},
                lastServerState: null
            });
        },

        createMasterDataModel: function () {
            return new JSONModel({
                persons: [],
                lpc: [],
                professions: [],
                statuses: ["SUCCESS", "WARNING", "CRITICAL"],
                resultTypes: ["PASS", "FAIL"],
                types: [],
                timezones: [
                    { key: "Europe/Amsterdam", text: "Europe/Amsterdam" },
                    { key: "UTC", text: "UTC" }
                ]
            });
        },

        createMplModel: function () {
            return new JSONModel({
                locations: []
            });
        },

        createEnvModel: function () {
            return new JSONModel({
                source: "defaults",
                loadedAt: "",
                timers: {
                    heartbeatMs: 240000,
                    lockStatusMs: 60000,
                    gcdMs: 300000,
                    idleMs: 600000,
                    autoSaveIntervalMs: 60000,
                    autoSaveDebounceMs: 30000,
                    networkGraceMs: 60000,
                    cacheFreshMs: 30000,
                    cacheStaleOkMs: 90000,
                    analyticsRefreshMs: 900000
                }
            });
        }

    };
});
