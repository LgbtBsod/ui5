sap.ui.define([], function () {
    "use strict";

    var mScopes = {};

    function normalizeScope(vScope) {
        return String(vScope || "").trim();
    }

    function mark(vScope) {
        var sScope = normalizeScope(vScope);
        if (!sScope) {
            return 0;
        }
        mScopes[sScope] = Number(mScopes[sScope] || 0) + 1;
        return mScopes[sScope];
    }

    function isCurrent(vScope, iToken) {
        var sScope = normalizeScope(vScope);
        if (!sScope || !iToken) {
            return true;
        }
        return Number(mScopes[sScope] || 0) === Number(iToken || 0);
    }

    function clear(vScope) {
        var sScope = normalizeScope(vScope);
        if (!sScope) {
            mScopes = {};
            return;
        }
        delete mScopes[sScope];
    }

    return {
        mark: mark,
        isCurrent: isCurrent,
        clear: clear
    };
});
