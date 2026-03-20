sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (FrontendConfigConstants, WorkflowContracts, WorkflowRuntimeConstants) {
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
        frontendConfigSource: WorkflowRuntimeConstants.SOURCES.GATEWAY,
        requestGroups: {
            save: WorkflowRuntimeConstants.REQUEST_GROUPS.SAVE_FLOW,
            autosave: WorkflowRuntimeConstants.REQUEST_GROUPS.AUTOSAVE,
            lock: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS,
            unlock: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS,
            lockHeartbeat: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS,
            lockStatus: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS,
            functionImport: WorkflowRuntimeConstants.REQUEST_GROUPS.LOCKS
        },
        readiness: {
            app: { status: WorkflowRuntimeConstants.READINESS_STATUS.PENDING, ready: false, readyAt: "", error: "" },
            search: { status: WorkflowRuntimeConstants.READINESS_STATUS.PENDING, ready: false, readyAt: "", error: "" },
            detail: { status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "", rootId: "", mode: WorkflowContracts.EDIT_MODES.READ, permissionKnown: false, lockKnown: false },
            analytics: { status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "" }
        },
        detailMeta: {
            rootId: "",
            readiness: { status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "" },
            mode: WorkflowContracts.EDIT_MODES.READ,
            lock: { state: WorkflowContracts.LOCK_STATES.READ_ONLY, known: false },
            dirty: false,
            permission: { known: false, allowed: false },
            save: { state: WorkflowContracts.AUTOSAVE_STATES.IDLE, lastSavedAt: null },
            validation: { state: WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE }
        },
        operationalKpiSnapshots: [],
        operationalKpiSnapshotLimit: 50,
        workflow: {
            detail: {
                editMode: WorkflowContracts.EDIT_MODES.READ,
                lock: { state: WorkflowContracts.LOCK_STATES.READ_ONLY },
                autosave: { state: WorkflowContracts.AUTOSAVE_STATES.IDLE, lastSavedAt: null }
            },
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
