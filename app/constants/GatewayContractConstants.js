sap.ui.define([], function () {
    "use strict";

    var FUNCTION_IMPORTS = Object.freeze({
        ANALYTICS_REFRESH_TRIGGER: "AnalyticsRefreshTrigger",
        AUTO_SAVE: "AutoSave",
        COPY_CHECKLIST: "CopyChecklist",
        CREATE_CHECKLIST: "CreateChecklist",
        GET_HIERARCHY: "GetHierarchy",
        LOCK_ACQUIRE: "LockAcquire",
        LOCK_HEARTBEAT: "LockHeartbeat",
        LOCK_RELEASE: "LockRelease",
        REPORT_EXPORT: "ReportExport",
        SAVE_CHANGES: "SaveChanges"
    });

    var ENTITY_SETS = Object.freeze({
        ANALYTICS_REFRESH_STATE: "AnalyticsRefreshStateSet",
        ATTACHMENT: "AttachmentSet",
        CHECKLIST_BARRIER: "ChecklistBarrierSet",
        CHECKLIST_BASIC_INFO: "ChecklistBasicInfoSet",
        CHECKLIST_CHECK: "ChecklistCheckSet",
        CHECKLIST_CREATE_PERMISSION: "ChecklistCreatePermissionSet",
        CHECKLIST_PERMISSION: "ChecklistPermissionSet",
        CHECKLIST_ROOT: "ChecklistRootSet",
        CHECKLIST_SEARCH: "ChecklistSearchSet",
        DICTIONARY_ITEM: "DictionaryItemSet",
        LAST_CHANGE_SET: "LastChangeSet",
        LOCK_STATUS: "LockStatusSet",
        PERSON_VALUE_HELP: "PersonVHSet",
        RUNTIME_SETTINGS: "RuntimeSettingsSet",
        SIMPLE_ANALYTICAL: "SimpleAnalyticalSet",
        WORKFLOW_ANALYTICS_BREAKDOWN: "WorkflowAnalyticsBreakdownSet"
    });

    var DISALLOWED_PATHS = Object.freeze([
        "FrontendRuntimeSettings",
        "capabilities",
        "ChecklistRoots",
        "SearchRows"
    ]);

    return {
        DISALLOWED_PATHS: DISALLOWED_PATHS,
        ENTITY_SETS: ENTITY_SETS,
        FUNCTION_IMPORTS: FUNCTION_IMPORTS
    };
});
