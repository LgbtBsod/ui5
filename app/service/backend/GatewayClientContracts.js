sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayContractConstants) {
    "use strict";

    function escapeRegExp(sValue) {
        return String(sValue || "").replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    }

    function exactPattern(sValue) {
        return new RegExp("^" + escapeRegExp(sValue) + "$", "i");
    }

    function entityDeletePattern(sEntitySet, sKeyPattern) {
        return new RegExp("^\\/" + escapeRegExp(sEntitySet) + "\\(" + sKeyPattern + "\\)$", "i");
    }

    function disallowedPathPattern(sTail) {
        return new RegExp("^\\/+" + sTail + "(?:$|[/?(])", "i");
    }

    return {
        DIRECT_DELETE_ALLOWLIST: [
            entityDeletePattern(GatewayContractConstants.ENTITY_SETS.CHECKLIST_ROOT, "(?:[^)]+)"),
            entityDeletePattern(GatewayContractConstants.ENTITY_SETS.CHECKLIST_CHECK, "(?:Key=)?[^)]+"),
            entityDeletePattern(GatewayContractConstants.ENTITY_SETS.CHECKLIST_BARRIER, "(?:Key=)?[^)]+"),
            entityDeletePattern(GatewayContractConstants.ENTITY_SETS.ATTACHMENT, "(?:AttachmentKey=)?[^)]+")
        ],
        DIRECT_FUNCTION_BODY_ALLOWLIST: [
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE)
        ],
        DIRECT_FUNCTION_QUERY_ALLOWLIST: [
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST),
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER)
        ],
        DIRECT_GET_FUNCTION_ALLOWLIST: [
            exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY)
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
