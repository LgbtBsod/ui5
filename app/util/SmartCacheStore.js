sap.ui.define([], function () {
    "use strict";

    var DB_NAME = "pcct_smart_cache";
    var STORE_NAME = "entries";

    function open() {
        return new Promise(function (resolve) {
            if (!window.indexedDB) { resolve(null); return; }
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

    function put(oDb, sKey, vValue, iFreshTs) {
        return new Promise(function (resolve) {
            if (!oDb) { resolve(false); return; }
            try {
                var oTx = oDb.transaction([STORE_NAME], "readwrite");
                oTx.objectStore(STORE_NAME).put({ key: sKey, value: vValue, freshness: iFreshTs || Date.now() });
                oTx.oncomplete = function () { resolve(true); };
                oTx.onerror = function () { resolve(false); };
            } catch (e) { resolve(false); }
        });
    }

    function get(oDb, sKey) {
        return new Promise(function (resolve) {
            if (!oDb) { resolve(null); return; }
            try {
                var oTx = oDb.transaction([STORE_NAME], "readonly");
                var oReq = oTx.objectStore(STORE_NAME).get(sKey);
                oReq.onsuccess = function () { resolve(oReq.result || null); };
                oReq.onerror = function () { resolve(null); };
            } catch (e) { resolve(null); }
        });
    }

    return { open: open, put: put, get: get };
});
