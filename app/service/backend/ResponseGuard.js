sap.ui.define([], function () {
    "use strict";

    var mScopes = {};
    var mActiveHandles = {};

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

    function replaceActiveHandle(vScope, iToken, oHandle) {
        var sScope = normalizeScope(vScope);
        var oPrevious;
        if (!sScope || !iToken || !oHandle) {
            return null;
        }
        oPrevious = mActiveHandles[sScope] || null;
        mActiveHandles[sScope] = {
            token: Number(iToken || 0),
            handle: oHandle
        };
        return oPrevious;
    }

    function clearActiveHandle(vScope, iToken, oHandle) {
        var sScope = normalizeScope(vScope);
        var oCurrent;
        if (!sScope) {
            mActiveHandles = {};
            return;
        }
        oCurrent = mActiveHandles[sScope];
        if (!oCurrent) {
            return;
        }
        if ((iToken && Number(oCurrent.token || 0) !== Number(iToken || 0)) ||
            (oHandle && oCurrent.handle !== oHandle)) {
            return;
        }
        delete mActiveHandles[sScope];
    }

    function clear(vScope) {
        var sScope = normalizeScope(vScope);
        if (!sScope) {
            mScopes = {};
            mActiveHandles = {};
            return;
        }
        delete mScopes[sScope];
        delete mActiveHandles[sScope];
    }

    return {
        mark: mark,
        isCurrent: isCurrent,
        replaceActiveHandle: replaceActiveHandle,
        clearActiveHandle: clearActiveHandle,
        clear: clear
    };
});
