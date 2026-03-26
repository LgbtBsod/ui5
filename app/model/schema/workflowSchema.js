sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (FrontendConfigConstants, SearchContracts, WorkflowContracts) {
    "use strict";

    return {
        filterId: "",
        filterLpc: "",
        search: { checksFailSegment: "ALL", barriersFailSegment: "ALL" },
        searchMode: SearchContracts.SEARCH_MODE.EXACT,
        searchBackendTop: SearchContracts.DEFAULTS.SEARCH_BACKEND_TOP,
        searchMaxResults: SearchContracts.DEFAULTS.SEARCH_VISIBLE_ROWS,
        searchFetchLimit: SearchContracts.DEFAULTS.SEARCH_BACKEND_TOP,
        growingPageSize: SearchContracts.DEFAULTS.SEARCH_VISIBLE_ROWS,
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
        frontendConfigSource: WorkflowContracts.SOURCES.GATEWAY,
        requestGroups: {
            save: WorkflowContracts.REQUEST_GROUPS.SAVE_FLOW,
            autosave: WorkflowContracts.REQUEST_GROUPS.AUTOSAVE,
            lock: WorkflowContracts.REQUEST_GROUPS.LOCKS,
            unlock: WorkflowContracts.REQUEST_GROUPS.LOCKS,
            lockHeartbeat: WorkflowContracts.REQUEST_GROUPS.LOCKS,
            lockStatus: WorkflowContracts.REQUEST_GROUPS.LOCKS,
            functionImport: WorkflowContracts.REQUEST_GROUPS.LOCKS
        },
        searchReturnContext: null,
        readiness: {
            app: { status: WorkflowContracts.READINESS_STATUS.PENDING, ready: false, readyAt: "", error: "" },
            search: { status: WorkflowContracts.READINESS_STATUS.PENDING, ready: false, readyAt: "", error: "" },
            detail: { status: WorkflowContracts.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "", rootId: "", activeObjectId: "", selectedId: "", mode: WorkflowContracts.EDIT_MODES.READ, permissionKnown: false, lockKnown: false },
            analytics: { status: WorkflowContracts.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "" }
        },
        detailMeta: {
            rootId: "",
            activeObjectId: "",
            selectedId: "",
            readiness: { status: WorkflowContracts.READINESS_STATUS.IDLE, ready: false, readyAt: "", error: "" },
            mode: WorkflowContracts.EDIT_MODES.READ,
            lock: { state: WorkflowContracts.LOCK_STATES.READ_ONLY, known: false },
            dirty: false,
            permission: { known: false, allowed: false },
            save: { state: WorkflowContracts.AUTOSAVE_STATES.IDLE, lastSavedAt: null },
            validation: { state: WorkflowContracts.VALIDATION_STATUS.IDLE }
        },
        operationalKpiSnapshots: [],
        operationalKpiSnapshotLimit: 50,
        workflow: {
            detail: {
                editMode: WorkflowContracts.EDIT_MODES.READ,
                lock: { state: WorkflowContracts.LOCK_STATES.READ_ONLY },
                autosave: { state: WorkflowContracts.AUTOSAVE_STATES.IDLE, lastSavedAt: null }
            }
        },
        operationalKpi: {
            saveAttempts: 0, saveSuccess: 0, saveFailed: 0,
            saveLatencyMsLast: 0, saveLatencyMsAvg: 0, saveLatencySamples: 0,
            conflictCount: 0, validationFailures: 0, retryFailures: 0,
            retryLatencyMsLast: 0, retryLatencyMsAvg: 0, retryLatencySamples: 0
        }
    };
});
