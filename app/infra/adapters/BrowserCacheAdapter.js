sap.ui.define([], function () {
    "use strict";

    var DB_NAME = "SmartCacheStore";
    var STORE = "snapshots";

    function openDb() {
        return new Promise(function (resolve, reject) {
            if (typeof indexedDB === "undefined") {
                reject(new Error("IndexedDB unavailable"));
                return;
            }
            var req = indexedDB.open(DB_NAME, 1);
            req.onupgradeneeded = function () { req.result.createObjectStore(STORE); };
            req.onsuccess = function () { resolve(req.result); };
            req.onerror = function () { reject(req.error || new Error("indexeddb_open_failed")); };
        });
    }

    function withStore(mode, fn) {
        return openDb().then(function (db) {
            return new Promise(function (resolve, reject) {
                var tx = db.transaction(STORE, mode);
                var st = tx.objectStore(STORE);
                Promise.resolve(fn(st)).then(resolve).catch(reject);
                tx.onerror = function () { reject(tx.error || new Error("indexeddb_tx_failed")); };
            }).finally(function () { db.close(); });
        });
    }

    function create() {
        return {
            read: function (sRootId) {
                return withStore("readonly", function (st) {
                    return new Promise(function (resolve, reject) {
                        var req = st.get(String(sRootId || ""));
                        req.onsuccess = function () { resolve(req.result || null); };
                        req.onerror = function () { reject(req.error || new Error("indexeddb_read_failed")); };
                    });
                });
            },
            write: function (sRootId, oData) {
                return withStore("readwrite", function (st) {
                    return new Promise(function (resolve, reject) {
                        var req = st.put(oData || null, String(sRootId || ""));
                        req.onsuccess = function () { resolve(true); };
                        req.onerror = function () { reject(req.error || new Error("indexeddb_write_failed")); };
                    });
                });
            },
            clear: function (sRootId) {
                return withStore("readwrite", function (st) {
                    return new Promise(function (resolve, reject) {
                        var req = st.delete(String(sRootId || ""));
                        req.onsuccess = function () { resolve(true); };
                        req.onerror = function () { reject(req.error || new Error("indexeddb_delete_failed")); };
                    });
                });
            }
        };
    }

    return { create: create };
});
