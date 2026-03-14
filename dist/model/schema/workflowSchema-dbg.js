sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/FrontendConfigConstants"
], function (FrontendConfigConstants) {
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
        searchFetchLimit: "100",
        growingPageSize: "100",
        exportLimit: 200000,
        searchSortKey: "DateCheck",
        searchSortDescending: true,
        searchGroupKey: "",
        searchGroupDescending: false,
        currentUser: {
            fullName: "",
            permissions: [],
            permissionRules: [],
            canView: false,
            canEdit: false,
            canDelete: false,
            summaryText: "",
            fetchedAt: ""
        },
        frontendVariables: Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES),
        frontendConfigSource: "gateway",
        requestGroups: { save: "saveFlow", autosave: "autosave", lock: "locks", unlock: "locks", lockHeartbeat: "locks", lockStatus: "locks", functionImport: "locks" },
        readiness: {
            app: { status: "pending", ready: false, readyAt: "", error: "" },
            search: { status: "pending", ready: false, readyAt: "", error: "" },
            detail: { status: "idle", ready: false, readyAt: "", error: "", rootId: "", mode: "READ", permissionKnown: false, lockKnown: false },
            analytics: { status: "idle", ready: false, readyAt: "", error: "" }
        },
        detailMeta: {
            rootId: "",
            readiness: { status: "idle", ready: false, readyAt: "", error: "" },
            mode: "READ",
            // Canonical lifecycle supports IDLE, READ_ONLY, ACQUIRING_LOCK, EDIT_LOCKED, LOCK_LOST,
            // IDLE_TIMEOUT_GRACE, and FORCED_READ_ONLY.
            lock: { state: "READ_ONLY", known: false },
            dirty: false,
            permission: { known: false, allowed: false },
            save: { state: "IDLE", lastSavedAt: null },
            validation: { state: "idle" }
        },
        operationalKpiSnapshots: [],
        operationalKpiSnapshotLimit: 50,
        workflow: {
            // Canonical lifecycle supports IDLE, READ_ONLY, ACQUIRING_LOCK, EDIT_LOCKED, LOCK_LOST,
            // IDLE_TIMEOUT_GRACE, and FORCED_READ_ONLY.
            detail: { editMode: "READ", lock: { state: "READ_ONLY" }, autosave: { state: "IDLE", lastSavedAt: null } },
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
