sap.ui.define([], function () {
    "use strict";

    function disallowedPathPattern(sTail) {
        return new RegExp("^\\/+" + sTail + "(?:$|[/?(])", "i");
    }

    return {
        DIRECT_DELETE_ALLOWLIST: [
            /^\/ChecklistRootSet\((?:[^)]+)\)$/i,
            /^\/AttachmentSet\((?:AttachmentKey=)?[^)]+\)$/i
        ],
        DIRECT_FUNCTION_BODY_ALLOWLIST: [
            /^SaveChanges$/i,
            /^AutoSave$/i,
            /^CreateChecklist$/i,
            /^ReportExport$/i,
            /* C-07 FIX: SetChecklistStatus was missing — caused runtime throw */
            /^SetChecklistStatus$/i
        ],
        DIRECT_FUNCTION_QUERY_ALLOWLIST: [
            /* C-06 TODO: LockAcquire/Heartbeat/Release use urlParameters (query string).
             * SessionGuid appears in Gateway access log (SM50).
             * Move to DIRECT_FUNCTION_BODY_ALLOWLIST after confirming ABAP accepts
             * POST body for these FunctionImports (they are declared m:HttpMethod="POST"). */
            /^LockAcquire$/i,
            /^LockHeartbeat$/i,
            /^LockRelease$/i,
            /^CopyChecklist$/i,
            /^AnalyticsRefreshTrigger$/i
        ],
        DIRECT_GET_FUNCTION_ALLOWLIST: [
            /^GetHierarchy$/i
        ],
        /* postToPath is reserved for future entity-POST endpoints.
         * C-08: DIRECT_POST_ALLOWLIST is intentionally empty — postToPath API is
         * currently unused. If a new POST endpoint is needed, add its path here. */
        DIRECT_POST_ALLOWLIST: [],
        FORBIDDEN_PATH_PATTERNS: [
            /^\/actions\//i,
            /^\/lock\//i,
            /^\/config\/frontend(?:$|[/?])/i,
            disallowedPathPattern("FrontendRuntimeSettings"),
            disallowedPathPattern("capabilities"),
            disallowedPathPattern("ChecklistRoots"),
            disallowedPathPattern("SearchRows"),
            disallowedPathPattern("ChecklistChecksSet"),
            disallowedPathPattern("ChecklistBarriersSet")
        ]
    };
});
