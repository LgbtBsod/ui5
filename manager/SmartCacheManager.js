sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    var FRESH_MS = 30 * 1000;
    var STALE_OK_MS = 90 * 1000;

    return EventProvider.extend("sap_ui5.manager.SmartCacheManager", {
        constructor: function () {
            EventProvider.apply(this, arguments);
            this._mL1 = {};
            this._mFreshness = {};
            this._mKeyMapping = {};
        },

        setKeyMapping: function (sTempKey, sRealKey) {
            this._mKeyMapping[sTempKey] = sRealKey;
            this.fireEvent("mappingChanged", { tempKey: sTempKey, realKey: sRealKey });
        },

        getRealKey: function (sTempKey) {
            return this._mKeyMapping[sTempKey] || sTempKey;
        },

        put: function (sKey, vValue) {
            this._mL1[sKey] = vValue;
            this._mFreshness[sKey] = Date.now();
            this.fireEvent("cacheUpdated", { key: sKey });
        },

        get: function (sKey) {
            return this._mL1[sKey];
        },

        getFreshnessState: function (sKey) {
            var iTs = this._mFreshness[sKey];
            if (!iTs) {
                return "MISS";
            }

            var iAge = Date.now() - iTs;
            if (iAge <= FRESH_MS) {
                return "FRESH";
            }

            if (iAge <= STALE_OK_MS) {
                return "STALE_OK";
            }

            return "STALE";
        },

        snapshot: function () {
            return {
                freshness: Object.assign({}, this._mFreshness),
                keyMapping: Object.assign({}, this._mKeyMapping)
            };
        }
    });
});
