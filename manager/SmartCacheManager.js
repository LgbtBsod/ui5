sap.ui.define([
    "sap/ui/base/EventProvider"
], function (EventProvider) {
    "use strict";

    var DEFAULT_FRESH_MS = 30 * 1000;
    // Cache validity window used by detail/edit flows.
    var DEFAULT_STALE_OK_MS = 90 * 1000;
    var DB_NAME = "pcct_smart_cache";
    var STORE_NAME = "entries";

    function _openDb() {
        return new Promise(function (resolve) {
            if (!window.indexedDB) {
                resolve(null);
                return;
            }
            var oReq = indexedDB.open(DB_NAME, 1);
            oReq.onupgradeneeded = function (oEvent) {
                var oDb = oEvent.target.result;
                if (!oDb.objectStoreNames.contains(STORE_NAME)) {
                    oDb.createObjectStore(STORE_NAME, { keyPath: "key" });
                }
            };
            oReq.onsuccess = function () { resolve(oReq.result); };
            oReq.onerror = function () { resolve(null); };
        });
    }

    function _idbPut(oDb, sKey, vValue, iFreshTs) {
        return new Promise(function (resolve) {
            if (!oDb) {
                resolve(false);
                return;
            }
            try {
                var oTx = oDb.transaction([STORE_NAME], "readwrite");
                oTx.objectStore(STORE_NAME).put({ key: sKey, value: vValue, freshness: iFreshTs || Date.now() });
                oTx.oncomplete = function () { resolve(true); };
                oTx.onerror = function () { resolve(false); };
            } catch (e) {
                resolve(false);
            }
        });
    }

    function _idbGet(oDb, sKey) {
        return new Promise(function (resolve) {
            if (!oDb) {
                resolve(null);
                return;
            }
            try {
                var oTx = oDb.transaction([STORE_NAME], "readonly");
                var oReq = oTx.objectStore(STORE_NAME).get(sKey);
                oReq.onsuccess = function () { resolve(oReq.result || null); };
                oReq.onerror = function () { resolve(null); };
            } catch (e) {
                resolve(null);
            }
        });
    }

    return EventProvider.extend("sap_ui5.manager.SmartCacheManager", {
        constructor: function (mOptions) {
            EventProvider.apply(this, arguments);
            this._iFreshMs = Number((mOptions && mOptions.freshMs) || DEFAULT_FRESH_MS);
            this._iStaleOkMs = Number((mOptions && mOptions.staleOkMs) || DEFAULT_STALE_OK_MS);
            this._mL1 = {};
            this._mFreshness = {};
            this._mKeyMapping = {};
            this._pDb = _openDb();
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
            this._pDb.then(function (oDb) {
                return _idbPut(oDb, sKey, vValue, this._mFreshness[sKey]);
            }.bind(this));
            this.fireEvent("cacheUpdated", { key: sKey });
        },

        get: function (sKey) {
            return this._mL1[sKey];
        },

        getWithFallback: function (sKey) {
            if (typeof this._mL1[sKey] !== "undefined") {
                return Promise.resolve(this._mL1[sKey]);
            }
            return this._pDb.then(function (oDb) {
                return _idbGet(oDb, sKey);
            }).then(function (oEntry) {
                if (!oEntry) {
                    return null;
                }
                this._mL1[sKey] = oEntry.value;
                this._mFreshness[sKey] = oEntry.freshness || Date.now();
                return oEntry.value;
            }.bind(this));
        },

        getFreshnessState: function (sKey) {
            var iTs = this._mFreshness[sKey];
            if (!iTs) {
                return "MISS";
            }

            var iAge = Date.now() - iTs;
            if (iAge <= this._iFreshMs) {
                return "FRESH";
            }

            if (iAge <= this._iStaleOkMs) {
                return "STALE_OK";
            }

            return "STALE";
        },

        isCacheValid: function (sKey) {
            var sState = this.getFreshnessState(sKey);
            return sState === "FRESH" || sState === "STALE_OK";
        },


        isCacheStrictFresh: function (sKey) {
            return this.getFreshnessState(sKey) === "FRESH";
        },

        configureFreshness: function (mFreshness) {
            var iFresh = Number(mFreshness && mFreshness.freshMs);
            var iStale = Number(mFreshness && mFreshness.staleOkMs);
            if (Number.isFinite(iFresh) && iFresh >= 1000) {
                this._iFreshMs = iFresh;
            }
            if (Number.isFinite(iStale) && iStale >= this._iFreshMs) {
                this._iStaleOkMs = iStale;
            }
        },

        snapshot: function () {
            return {
                freshness: Object.assign({}, this._mFreshness),
                keyMapping: Object.assign({}, this._mKeyMapping)
            };
        }
    });
});
