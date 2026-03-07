sap.ui.define([
    "sap/ui/base/EventProvider",
    "checklist/app/util/SmartCacheStore",
    "checklist/app/util/SmartCacheUtils"
], function (EventProvider, SmartCacheStore, SmartCacheUtils) {
    "use strict";

    var DEFAULT_FRESH_MS = 30 * 1000;
    var DEFAULT_STALE_OK_MS = 90 * 1000;
    var DEFAULT_ALLOWED_KEYS = ["checkLists"];

    return EventProvider.extend("checklist.app.service.runtime.SmartCacheManager", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iFreshMs = Number((mOptions && mOptions.freshMs) || DEFAULT_FRESH_MS);
            this._iStaleOkMs = Number((mOptions && mOptions.staleOkMs) || DEFAULT_STALE_OK_MS);
            this._mAllowedKeys = SmartCacheUtils.buildAllowedMap((mOptions && mOptions.allowedKeys) || DEFAULT_ALLOWED_KEYS);
            this._mL1 = {}; this._mFreshness = {}; this._mKeyMapping = {}; this._pDb = SmartCacheStore.open();
        },
        _isKeyAllowed: function (sKey) { return !!this._mAllowedKeys[String(sKey || "")]; },
        setKeyMapping: function (sTempKey, sRealKey) { this._mKeyMapping[sTempKey] = sRealKey; this.fireEvent("mappingChanged", { tempKey: sTempKey, realKey: sRealKey }); },
        getRealKey: function (sTempKey) { return this._mKeyMapping[sTempKey] || sTempKey; },
        put: function (sKey, vValue) {
            if (!this._isKeyAllowed(sKey)) { this.fireEvent("cacheSkipped", { key: sKey, reason: "not_allowed" }); return false; }
            this._mL1[sKey] = vValue; this._mFreshness[sKey] = Date.now();
            this._pDb.then(function (oDb) { return SmartCacheStore.put(oDb, sKey, vValue, this._mFreshness[sKey]); }.bind(this));
            this.fireEvent("cacheUpdated", { key: sKey }); return true;
        },
        get: function (sKey) { return this._isKeyAllowed(sKey) ? this._mL1[sKey] : undefined; },
        getCached: function (sKey) {
            if (!this._isKeyAllowed(sKey)) { this.fireEvent("cacheSkipped", { key: sKey, reason: "not_allowed" }); return Promise.resolve(null); }
            if (typeof this._mL1[sKey] !== "undefined") { return Promise.resolve(this._mL1[sKey]); }
            return this._pDb.then(function (oDb) { return SmartCacheStore.get(oDb, sKey); }).then(function (oEntry) {
                if (!oEntry) { return null; }
                this._mL1[sKey] = oEntry.value; this._mFreshness[sKey] = oEntry.freshness || Date.now(); return oEntry.value;
            }.bind(this));
        },
        getFreshnessState: function (sKey) { return SmartCacheUtils.freshnessState(this._mAllowedKeys, this._mFreshness, sKey, this._iFreshMs, this._iStaleOkMs); },
        isCacheValid: function (sKey) { var sState = this.getFreshnessState(sKey); return sState === "FRESH" || sState === "STALE_OK"; },
        isCacheStrictFresh: function (sKey) { return this.getFreshnessState(sKey) === "FRESH"; },
        configureFreshness: function (mFreshness) {
            var iFresh = Number(mFreshness && mFreshness.freshMs); var iStale = Number(mFreshness && mFreshness.staleOkMs);
            if (Number.isFinite(iFresh) && iFresh >= 1000) { this._iFreshMs = iFresh; }
            if (Number.isFinite(iStale) && iStale >= this._iFreshMs) { this._iStaleOkMs = iStale; }
        },
        snapshot: function () { return { freshness: Object.assign({}, this._mFreshness), keyMapping: Object.assign({}, this._mKeyMapping), allowedKeys: Object.keys(this._mAllowedKeys) }; }
    });
});
