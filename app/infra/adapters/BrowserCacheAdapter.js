sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (StatePaths) {
    "use strict";

    var DB_NAME = "PcctSessionCache";
    var DB_VERSION = 2;
    var STORE = "entries";
    var DEFAULT_ENTITY_KIND = "detailSnapshot";
    var SCHEMA_VERSION = 2;

    function nowIso() {
        return new Date().toISOString();
    }

    function safeSessionGet(sKey) {
        try {
            return window.sessionStorage.getItem(sKey) || "";
        } catch (e) {
            return "";
        }
    }

    function safeSessionSet(sKey, sValue) {
        try {
            window.sessionStorage.setItem(sKey, sValue);
        } catch (e) {
            return;
        }
    }

    function buildFallbackTabSessionId() {
        return "T" + Math.random().toString(36).slice(2) + Date.now().toString(36);
    }

    function normalizeEntityKind(sEntityKind) {
        return String(sEntityKind || DEFAULT_ENTITY_KIND).trim() || DEFAULT_ENTITY_KIND;
    }

    function normalizeRootId(sRootId) {
        return String(sRootId || "").trim();
    }

    function buildStoreKey(sTabSessionId, sEntityKind, sRootId) {
        return [String(sTabSessionId || "").trim(), normalizeEntityKind(sEntityKind), normalizeRootId(sRootId)].join("|");
    }

    function requestAsPromise(oRequest, sErrorCode) {
        return new Promise(function (resolve, reject) {
            oRequest.onsuccess = function () {
                resolve(oRequest.result);
            };
            oRequest.onerror = function () {
                reject(oRequest.error || new Error(sErrorCode));
            };
        });
    }

    function openDb() {
        return new Promise(function (resolve, reject) {
            if (typeof indexedDB === "undefined") {
                reject(new Error("IndexedDB unavailable"));
                return;
            }
            var req = indexedDB.open(DB_NAME, DB_VERSION);
            req.onupgradeneeded = function () {
                var oDb = req.result;
                if (oDb.objectStoreNames.contains(STORE)) {
                    oDb.deleteObjectStore(STORE);
                }
                var oStore = oDb.createObjectStore(STORE, {
                    keyPath: "storeKey"
                });
                oStore.createIndex("byTabSessionId", "tabSessionId", {
                    unique: false
                });
                oStore.createIndex("byEntityRoot", ["tabSessionId", "entityKind", "rootId"], {
                    unique: true
                });
            };
            req.onsuccess = function () { resolve(req.result); };
            req.onerror = function () { reject(req.error || new Error("indexeddb_open_failed")); };
        });
    }

    function withStore(mode, fn) {
        return openDb().then(function (db) {
            return new Promise(function (resolve, reject) {
                var tx = db.transaction(STORE, mode);
                var st = tx.objectStore(STORE);
                var vResult;
                var bSettled = false;

                function fail(oError) {
                    if (bSettled) {
                        return;
                    }
                    bSettled = true;
                    reject(oError);
                }

                tx.oncomplete = function () {
                    if (bSettled) {
                        return;
                    }
                    bSettled = true;
                    resolve(vResult);
                };
                tx.onerror = function () {
                    fail(tx.error || new Error("indexeddb_tx_failed"));
                };
                tx.onabort = function () {
                    fail(tx.error || new Error("indexeddb_tx_aborted"));
                };
                Promise.resolve(fn(st, tx)).then(function (vResolved) {
                    vResult = vResolved;
                }).catch(function (oError) {
                    try {
                        tx.abort();
                    } catch (e) {
                        // Ignore abort errors; transaction completion handlers will reject.
                    }
                    fail(oError);
                });
            }).then(function (vResult) { db.close(); return vResult; }, function (oError) { db.close(); throw oError; });
        });
    }

    function create(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;

        function resolveTabSessionId() {
            var sStateValue = oStateModel && oStateModel.getProperty
                ? String(oStateModel.getProperty(StatePaths.TAB_SESSION_ID) || "").trim()
                : "";
            var sStored = safeSessionGet("pcct_tab_session_id");
            var sResolved = sStateValue || sStored || buildFallbackTabSessionId();
            if (!sStored) {
                safeSessionSet("pcct_tab_session_id", sResolved);
            }
            if (oStateModel && oStateModel.setProperty && !sStateValue) {
                oStateModel.setProperty(StatePaths.TAB_SESSION_ID, sResolved);
            }
            return sResolved;
        }

        return {
            read: function (sRootId, sEntityKind) {
                var sResolvedRootId = normalizeRootId(sRootId);
                var sResolvedEntityKind = normalizeEntityKind(sEntityKind);
                var sTabSessionId = resolveTabSessionId();
                if (!sResolvedRootId || !sTabSessionId) {
                    return Promise.resolve(null);
                }
                return withStore("readonly", function (st) {
                    return requestAsPromise(
                        st.get(buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedRootId)),
                        "indexeddb_read_failed"
                    ).then(function (oEntry) {
                        return oEntry || null;
                    });
                });
            },
            write: function (sRootId, oData, mMeta) {
                var sResolvedRootId = normalizeRootId(sRootId);
                var sResolvedEntityKind = normalizeEntityKind(mMeta && mMeta.entityKind);
                var sTabSessionId = resolveTabSessionId();
                var sStoreKey = buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedRootId);
                if (!sResolvedRootId || !sTabSessionId) {
                    return Promise.resolve(null);
                }
                return withStore("readwrite", function (st) {
                    return requestAsPromise(
                        st.get(sStoreKey),
                        "indexeddb_read_existing_failed"
                    ).then(function (oExisting) {
                        var oMeta = mMeta || {};
                        var sCreatedAt = String((oExisting && oExisting.createdAt) || oMeta.createdAt || nowIso()).trim();
                        var sValidatedAt = String(oMeta.validatedAt || nowIso()).trim();
                        var oRecord = {
                            storeKey: sStoreKey,
                            tabSessionId: sTabSessionId,
                            entityKind: sResolvedEntityKind,
                            rootId: sResolvedRootId,
                            payload: oData || null,
                            lastChangeSet: String(oMeta.lastChangeSet || "").trim(),
                            createdAt: sCreatedAt,
                            validatedAt: sValidatedAt,
                            schemaVersion: Number(oMeta.schemaVersion || SCHEMA_VERSION) || SCHEMA_VERSION
                        };
                        st.put(oRecord);
                        return oRecord;
                    });
                });
            },
            clear: function (sRootId, sEntityKind) {
                var sResolvedRootId = normalizeRootId(sRootId);
                var sResolvedEntityKind = normalizeEntityKind(sEntityKind);
                var sTabSessionId = resolveTabSessionId();
                if (!sResolvedRootId || !sTabSessionId) {
                    return Promise.resolve(false);
                }
                return withStore("readwrite", function (st) {
                    st.delete(buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedRootId));
                    return true;
                });
            }
        };
    }

    return { create: create };
});
