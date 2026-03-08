sap.ui.define([], function () {
    "use strict";

    return {
        filterId: "",
        filterLpc: "",
        filterFailedChecks: "ALL",
        filterFailedBarriers: "ALL",
        search: { checksFailSegment: "ALL", barriersFailSegment: "ALL" },
        searchMode: "EXACT",
        searchBackendTop: "100",
        searchMaxResults: "100",
        currentUser: {
            uname: "",
            fullName: "",
            permissions: [],
            permissionRules: [],
            canView: false,
            canEdit: false,
            canDelete: false,
            summaryText: "",
            fetchedAt: ""
        },
        frontendVariables: {},
        frontendConfigSource: "gateway",
        requestGroups: { save: "saveFlow", autosave: "autosave", lock: "locks", unlock: "locks", lockHeartbeat: "locks", lockStatus: "locks", functionImport: "locks" },
        operationalKpiSnapshots: [],
        operationalKpiSnapshotLimit: 50,
        workflow: {
            detail: { editMode: "READ", lock: { state: "IDLE" }, autosave: { state: "IDLE", lastSavedAt: null } },
            search: { mode: "EXACT", segments: { checks: "ALL", barriers: "ALL" } }
        },
        operationalKpi: {
            saveAttempts: 0, saveSuccess: 0, saveFailed: 0,
            saveLatencyMsLast: 0, saveLatencyMsAvg: 0, saveLatencySamples: 0,
            conflictCount: 0, validationFailures: 0, retryFailures: 0,
            retryLatencyMsLast: 0, retryLatencyMsAvg: 0, retryLatencySamples: 0
        }
    };
});
