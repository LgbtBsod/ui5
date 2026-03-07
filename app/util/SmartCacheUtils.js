sap.ui.define([], function () {
    "use strict";

    function buildAllowedMap(aAllowed) {
        return (aAllowed || []).reduce(function (mAcc, sKey) {
            mAcc[String(sKey)] = true;
            return mAcc;
        }, {});
    }

    function freshnessState(mAllowedKeys, mFreshness, sKey, iFreshMs, iStaleOkMs) {
        if (!mAllowedKeys[String(sKey || "")]) { return "MISS"; }
        var iTs = mFreshness[sKey];
        if (!iTs) { return "MISS"; }
        var iAge = Date.now() - iTs;
        if (iAge <= iFreshMs) { return "FRESH"; }
        if (iAge <= iStaleOkMs) { return "STALE_OK"; }
        return "STALE";
    }

    return {
        buildAllowedMap: buildAllowedMap,
        freshnessState: freshnessState
    };
});
