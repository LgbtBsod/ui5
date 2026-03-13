sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts"
], function (GatewayClientContracts) {
    "use strict";

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

    return {
        allowlisted: allowlisted,
        assertAllowedFunctionName: assertAllowedFunctionName,
        assertAllowedPath: assertAllowedPath,
        assertCanonicalPath: assertCanonicalPath,
        buildHeaders: buildHeaders,
        encodeUrlParameters: encodeUrlParameters,
        normalizePath: normalizePath
    };
});
