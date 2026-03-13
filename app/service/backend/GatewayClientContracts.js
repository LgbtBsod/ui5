sap.ui.define([], function () {
    "use strict";

    function disallowedPathPattern(sTail) {
        return new RegExp("^\\/" + sTail + "(?:$|[/?(])", "i");
    }

    return {
        DIRECT_DELETE_ALLOWLIST: [
            /^\/ChecklistRootSet\((?:[^)]+)\)$/i,
            /^\/AttachmentSet\((?:AttachmentKey=)?[^)]+\)$/i
        ],
        DIRECT_FUNCTION_BODY_ALLOWLIST: [],
        DIRECT_FUNCTION_QUERY_ALLOWLIST: [
            /^LockAcquire$/i,
            /^LockHeartbeat$/i,
            /^LockRelease$/i,
            /^CopyChecklist$/i,
            /^AnalyticsRefreshTrigger$/i
        ],
        DIRECT_GET_FUNCTION_ALLOWLIST: [
            /^GetHierarchy$/i
        ],
        DIRECT_POST_ALLOWLIST: [
            /^\/CreateChecklist(?:$|[?(])/i,
            /^\/AutoSave(?:$|[?(])/i,
            /^\/SaveChanges(?:$|[?(])/i,
            /^\/ReportExport(?:$|[?(])/i
        ],
        FORBIDDEN_PATH_PATTERNS: [
            /^\/actions\//i,
            /^\/lock\//i,
            /^\/config\/frontend(?:$|[/?])/i,
            disallowedPathPattern("Front" + "endRuntimeSettings"),
            disallowedPathPattern("cap" + "abilities"),
            disallowedPathPattern("Checklist" + "Roots"),
            disallowedPathPattern("Search" + "Rows"),
            disallowedPathPattern("Checklist" + "ChecksSet"),
            disallowedPathPattern("Checklist" + "BarriersSet")
        ]
    };
});
