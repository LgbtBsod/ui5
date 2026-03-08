sap.ui.define([], function () {
    "use strict";

    var SCOPE_DEFAULT = "default";
    var SCOPE_OVERRIDE = "override";
    var mScopes = Object.create(null);

    function normalizeValue(vValue) {
        return String(vValue || "").trim();
    }

    function normalizeScope(sScope) {
        return normalizeValue(sScope);
    }

    function normalizeKind(sKind) {
        return normalizeValue(sKind) || SCOPE_OVERRIDE;
    }

    function ensureBucket(sScope, sKind) {
        var sNormalizedScope = normalizeScope(sScope);
        var sNormalizedKind = normalizeKind(sKind);
        if (!sNormalizedScope) {
            return null;
        }
        if (!mScopes[sNormalizedScope]) {
            mScopes[sNormalizedScope] = Object.create(null);
        }
        if (!mScopes[sNormalizedScope][sNormalizedKind]) {
            mScopes[sNormalizedScope][sNormalizedKind] = Object.create(null);
        }
        return mScopes[sNormalizedScope][sNormalizedKind];
    }

    function register(sScope, sKind, sId, fnHandler) {
        var oBucket;
        var sNormalizedId = normalizeValue(sId);
        if (!sNormalizedId || typeof fnHandler !== "function") {
            return false;
        }
        oBucket = ensureBucket(sScope, sKind);
        if (!oBucket) {
            return false;
        }
        oBucket[sNormalizedId] = fnHandler;
        return true;
    }

    function unregister(sScope, sKind, sId) {
        var oBucket = ensureBucket(sScope, sKind);
        var sNormalizedId = normalizeValue(sId);
        if (!oBucket || !sNormalizedId || !oBucket[sNormalizedId]) {
            return false;
        }
        delete oBucket[sNormalizedId];
        return true;
    }

    function resolve(sScope, sKind, aIds) {
        var oBucket = ensureBucket(sScope, sKind);
        var aCandidates = Array.isArray(aIds) ? aIds : [];
        var iIndex;
        var sId;
        if (!oBucket) {
            return null;
        }
        for (iIndex = 0; iIndex < aCandidates.length; iIndex += 1) {
            sId = normalizeValue(aCandidates[iIndex]);
            if (sId && oBucket[sId]) {
                return {
                    id: sId,
                    handler: oBucket[sId]
                };
            }
        }
        return null;
    }

    function clearScope(sScope, sKind) {
        var sNormalizedScope = normalizeScope(sScope);
        var sNormalizedKind = normalizeKind(sKind);
        if (!sNormalizedScope || !mScopes[sNormalizedScope] || !mScopes[sNormalizedScope][sNormalizedKind]) {
            return false;
        }
        mScopes[sNormalizedScope][sNormalizedKind] = Object.create(null);
        return true;
    }

    return {
        registerDefault: function (sScope, sId, fnHandler) {
            return register(sScope, SCOPE_DEFAULT, sId, fnHandler);
        },
        registerOverride: function (sScope, sId, fnHandler) {
            return register(sScope, SCOPE_OVERRIDE, sId, fnHandler);
        },
        unregisterDefault: function (sScope, sId) {
            return unregister(sScope, SCOPE_DEFAULT, sId);
        },
        unregisterOverride: function (sScope, sId) {
            return unregister(sScope, SCOPE_OVERRIDE, sId);
        },
        resolveDefault: function (sScope, aIds) {
            return resolve(sScope, SCOPE_DEFAULT, aIds);
        },
        resolveOverride: function (sScope, aIds) {
            return resolve(sScope, SCOPE_OVERRIDE, aIds);
        },
        clearDefaults: function (sScope) {
            return clearScope(sScope, SCOPE_DEFAULT);
        },
        clearOverrides: function (sScope) {
            return clearScope(sScope, SCOPE_OVERRIDE);
        }
    };
});
