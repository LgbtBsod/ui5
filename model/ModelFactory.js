sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function (JSONModel) {
    "use strict";

    function createTimers() {
        return {
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
        };
    }

    return {

        createDataModel: function () {
            return new JSONModel({
                checkLists: [],
                visibleCheckLists: [],
                selectedChecklist: null
            });
        },

        // New canonical models (migration-safe addition)
        createUiStateModel: function () {
            return new JSONModel({
                mode: "READ",
                busy: false,
                currentRootKey: "",
                sessionGuid: "",
                lock: { ok: false, reason: "FREE", isKilled: false },
                timers: createTimers(),
                activity: { lastActiveAt: "", idleUntil: "" }
            });
        },

        createViewModel: function () {
            return new JSONModel({
                root: {},
                basicInfo: {},
                checks: { items: [] },
                barriers: { items: [] },
                attachments: { items: [] },
                meta: { aggChangedOn: "" }
            });
        },

        createCacheModel: function () {
            return new JSONModel({
                // canonical cache storage
                byRootKey: {},
                // legacy props kept to avoid breakage
                pristineSnapshot: null,
                keyMapping: {},
                lastServerState: null
            });
        },

        createMasterDataModel: function () {
            return new JSONModel({
                // canonical dict storage
                dict: {},
                runtime: { timers: {}, requiredFields: [], uploadPolicy: { scan: "always-ok" } },
                // legacy props used by existing controllers
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

        createHierarchyModel: function () {
            return new JSONModel({
                byDate: {}
            });
        },

        // Legacy model used throughout existing app code
        createStateModel: function () {
            return new JSONModel({
                mode: "READ",
                layout: "OneColumn",
                preferredDetailLayout: "TwoColumnsMidExpanded",
                columnSplitPercent: 38,
                isLoading: false,
                loadError: false,
                loadErrorMessage: "",
                filterId: "",
                filterLpc: "",
                filterFailedChecks: "ALL",
                filterFailedBarriers: "ALL",
                searchMode: "EXACT",
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
                frontendVariables: {},
                frontendConfigSource: "defaults",
                mainServiceMetadataOk: null,
                mainServiceMetadataError: "",
                capabilityStatus: "pending",
                capabilityDegradedReason: "",
                capabilityMessageKey: "capabilityPending",
                capabilityDiagnostics: {},
                requestGroups: {
                    save: "saveFlow",
                    autosave: "autosave",
                    lock: "locks",
                    unlock: "locks",
                    lockHeartbeat: "locks",
                    lockStatus: "locks",
                    functionImport: "locks"
                },
                timers: createTimers(),
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

        createMplModel: function () {
            return new JSONModel({
                locations: []
            });
        },

        createEnvModel: function () {
            return new JSONModel({
                source: "defaults",
                loadedAt: "",
                variables: {},
                timers: createTimers()
            });
        }

    };
});
