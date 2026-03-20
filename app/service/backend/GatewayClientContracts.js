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
            /^SetChecklistStatus$/i,
            /* C-06 FIX: moved from QUERY to BODY — metadata confirms m:HttpMethod="POST".
             * Using model.create() sends payload in request body instead of URL query string,
             * which prevents SessionGuid from appearing in SAP Gateway access log (SM50/SMICM). */
            /^LockAcquire$/i,
            /^LockHeartbeat$/i,
            /^LockRelease$/i
        ],
        DIRECT_FUNCTION_QUERY_ALLOWLIST: [
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
