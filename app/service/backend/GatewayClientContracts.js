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

    function normalizePath(sPath) {
        var sNormalized = String(sPath || "");
        return sNormalized.charAt(0) === "/" ? sNormalized : ("/" + sNormalized);
    }

    function allowlisted(sValue, aAllowed) {
        return (aAllowed || []).some(function (oPattern) {
            return oPattern.test(sValue);
        });
    }

    function assertCanonicalPath(sPath) {
        GatewayClientContracts.FORBIDDEN_PATH_PATTERNS.forEach(function (oPattern) {
            if (oPattern.test(sPath)) {
                throw new Error("Forbidden non-canonical OData path: " + sPath);
            }
        });
        return sPath;
    }

    function assertAllowedPath(sPath, aAllowed, sOperation) {
        if (!allowlisted(sPath, aAllowed || [])) {
            throw new Error("Unsupported " + sOperation + " OData path: " + sPath);
        }
        return sPath;
    }

    function assertAllowedFunctionName(sName) {
        var sResolved = String(sName || "").trim();
        if (!sResolved) {
            throw new Error("Function import name is required");
        }
        if (GatewayClientContracts.FORBIDDEN_PATH_PATTERNS.some(function (oPattern) { return oPattern.test("/" + sResolved); })) {
            throw new Error("Forbidden non-canonical function import: " + sResolved);
        }
        return sResolved;
    }

    function encodeUrlParameters(mParameters) {
        return Object.keys(mParameters || {}).reduce(function (aPairs, sKey) {
            var vValue = mParameters[sKey];
            if (vValue === undefined || vValue === null || vValue === "") {
                return aPairs;
            }
            aPairs.push(encodeURIComponent(sKey) + "=" + encodeURIComponent(String(vValue)));
            return aPairs;
        }, []).join("&");
    }

    function buildHeaders(mHeaders, sCorrelationId) {
        var mResolved = Object.assign({}, mHeaders || {});
        if (sCorrelationId) {
            mResolved["X-Correlation-ID"] = sCorrelationId;
            mResolved["X-Request-ID"] = sCorrelationId;
        }
        return mResolved;
    }

    var GatewayClientContracts = {
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

    GatewayClientContracts.allowlisted = allowlisted;
    GatewayClientContracts.assertAllowedFunctionName = assertAllowedFunctionName;
    GatewayClientContracts.assertAllowedPath = assertAllowedPath;
    GatewayClientContracts.assertCanonicalPath = assertCanonicalPath;
    GatewayClientContracts.buildHeaders = buildHeaders;
    GatewayClientContracts.encodeUrlParameters = encodeUrlParameters;
    GatewayClientContracts.normalizePath = normalizePath;

    return GatewayClientContracts;
});
