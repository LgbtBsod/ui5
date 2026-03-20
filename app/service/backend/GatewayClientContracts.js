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
        /* Reserved for future direct-POST entity endpoints. Currently unused. */
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
