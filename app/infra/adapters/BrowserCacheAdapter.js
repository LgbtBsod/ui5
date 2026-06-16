sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (StatePaths) {
    "use strict";

    /* Этот блок описывает контракт session-кэша detail snapshot в IndexedDB.
     * Результат: записи кэша изолированы по вкладке и не смешиваются с постоянными настройками. */
    var DB_NAME = "PcctSessionCache";
    var DB_VERSION = 2;
    var STORE = "entries";
    var DEFAULT_ENTITY_KIND = "detailSnapshot";
    var SCHEMA_VERSION = 2;

    /* Этот блок возвращает текущее время в ISO-формате для метаданных кэша.
     * Результат: createdAt/validatedAt записываются единообразно. */
    function nowIso() {
        return new Date().toISOString();
    }

    /* Этот блок безопасно читает значение из sessionStorage.
     * Результат: отсутствие доступа к storage не роняет приложение. */
    function safeSessionGet(sKey) {
        try {
            return window.sessionStorage.getItem(sKey) || "";
        } catch (e) {
            return "";
        }
    }

    /* Этот блок безопасно пишет значение в sessionStorage.
     * Результат: деградация storage не ломает runtime-flow. */
    function safeSessionSet(sKey, sValue) {
        try {
            window.sessionStorage.setItem(sKey, sValue);
        } catch (e) {
            return;
        }
    }

    /* Этот блок создает fallback tabSessionId для изоляции кэша внутри текущей вкладки.
     * Результат: записи разных вкладок не пересекаются.
     * Заменяет прошлый Math.random()-fallback на криптостойкий crypto.getRandomValues
     * с деградацией до Math.random для legacy-окружений. */
    function buildFallbackTabSessionId() {
        var sRandom = "0000";
        try {
            if (typeof crypto !== "undefined" && typeof crypto.getRandomValues === "function") {
                var oBytes = new Uint8Array(16);
                crypto.getRandomValues(oBytes);
                sRandom = Array.prototype.map.call(oBytes, function (nByte) {
                    return nByte.toString(16).padStart(2, "0");
                }).join("");
            }
        } catch (_cryptoError) {
            sRandom = Math.random().toString(36).slice(2);
        }
        return "T" + sRandom + Date.now().toString(36);
    }

    function normalizeEntityKind(sEntityKind) {
        return String(sEntityKind || DEFAULT_ENTITY_KIND).trim() || DEFAULT_ENTITY_KIND;
    }

    function normalizeDbKey(sDbKey) {
        return String(sDbKey || "").trim();
    }

    function buildStoreKey(sTabSessionId, sEntityKind, sDbKey) {
        return [String(sTabSessionId || "").trim(), normalizeEntityKind(sEntityKind), normalizeDbKey(sDbKey)].join("|");
    }

    /* Этот блок оборачивает IndexedDB request в Promise.
     * Результат: read/write операции дальше можно собирать в последовательный async flow. */
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

    /* Этот блок открывает IndexedDB и при необходимости пересоздает схему store/indexes.
     * Результат: session cache всегда работает на ожидаемой версии структуры.
     * Кэширует открытое соединение в _dbPromise; сбрасывает кэш при ошибке/закрытии,
     * чтобы следующий вызов мог повторить попытку и не зависнуть на versionchange. */
    var _dbPromise = null;
    function openDb() {
        if (_dbPromise) { return _dbPromise; }
        _dbPromise = new Promise(function (resolve, reject) {
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
            req.onblocked = function () { reject(new Error("indexeddb_open_blocked")); };
        }).catch(function (oError) {
            _dbPromise = null;
            throw oError;
        });
        return _dbPromise;
    }

    /* Этот блок дает единый способ выполнить операцию внутри transaction/store.
     * Результат: readwrite/readonly действия завершаются с контролируемым resolve/reject.
     * Соединение закрывается на settle и кэш сбрасывается, чтобы не блокировать
     * последующие versionchange-апгрейды схемы. */
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
            }).then(
                function (vResult) { try { db.close(); } catch (e) { /* ignore */ } _dbPromise = null; return vResult; },
                function (oError) { try { db.close(); } catch (e) { /* ignore */ } _dbPromise = null; throw oError; }
            );
        });
    }

    /* Этот блок итерирует индекс через курсор.
     * Результат: можно массово чистить или анализировать записи без дублирования кода. */
    function iterateIndex(oIndex, fnVisit) {
        return new Promise(function (resolve, reject) {
            var oRequest = oIndex.openCursor();

            oRequest.onsuccess = function () {
                var oCursor = oRequest.result;
                if (!oCursor) {
                    resolve();
                    return;
                }
                Promise.resolve(fnVisit(oCursor)).then(function () {
                    oCursor.continue();
                }).catch(function (oError) {
                    reject(oError);
                });
            };
            oRequest.onerror = function () {
                reject(oRequest.error || new Error("indexeddb_cursor_failed"));
            };
        });
    }

    /* Этот блок создает adapter instance для session cache.
     * Результат: фронт получает единый owner для read/write/cleanup snapshot-кэша. */
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

        function clearByTabSessionId(sTabSessionId) {
            var sResolvedTabSessionId = String(sTabSessionId || "").trim();
            if (!sResolvedTabSessionId) {
                return Promise.resolve({ cleared: 0, tabSessionId: "" });
            }
            return withStore("readwrite", function (st) {
                var oIndex = st.index("byTabSessionId");
                var iCleared = 0;
                return iterateIndex(oIndex, function (oCursor) {
                    var oValue = oCursor && oCursor.value || {};
                    if (String(oValue.tabSessionId || "").trim() !== sResolvedTabSessionId) {
                        return null;
                    }
                    iCleared += 1;
                    oCursor.delete();
                    return null;
                }).then(function () {
                    return {
                        cleared: iCleared,
                        tabSessionId: sResolvedTabSessionId
                    };
                });
            });
        }

        function cleanupStaleSessions(sActiveTabSessionId) {
            var sResolvedActiveTabSessionId = String(sActiveTabSessionId || resolveTabSessionId()).trim();
            if (!sResolvedActiveTabSessionId) {
                return Promise.resolve({ cleared: 0, activeTabSessionId: "" });
            }
            return withStore("readwrite", function (st) {
                var oIndex = st.index("byTabSessionId");
                var iCleared = 0;
                return iterateIndex(oIndex, function (oCursor) {
                    var oValue = oCursor && oCursor.value || {};
                    var sEntryTabSessionId = String(oValue.tabSessionId || "").trim();
                    if (!sEntryTabSessionId || sEntryTabSessionId === sResolvedActiveTabSessionId) {
                        return null;
                    }
                    iCleared += 1;
                    oCursor.delete();
                    return null;
                }).then(function () {
                    return {
                        cleared: iCleared,
                        activeTabSessionId: sResolvedActiveTabSessionId
                    };
                });
            });
        }

        return {
            read: function (sDbKey, sEntityKind) {
                var sResolvedDbKey = normalizeDbKey(sDbKey);
                var sResolvedEntityKind = normalizeEntityKind(sEntityKind);
                var sTabSessionId = resolveTabSessionId();
                if (!sResolvedDbKey || !sTabSessionId) {
                    return Promise.resolve(null);
                }
                return withStore("readonly", function (st) {
                    return requestAsPromise(
                        st.get(buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedDbKey)),
                        "indexeddb_read_failed"
                    ).then(function (oEntry) {
                        return oEntry || null;
                    });
                });
            },
            write: function (sDbKey, oData, mMeta) {
                var sResolvedDbKey = normalizeDbKey(sDbKey);
                var sResolvedEntityKind = normalizeEntityKind(mMeta && mMeta.entityKind);
                var sTabSessionId = resolveTabSessionId();
                var sStoreKey = buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedDbKey);
                if (!sResolvedDbKey || !sTabSessionId) {
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
                            rootId: sResolvedDbKey,
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
            clear: function (sDbKey, sEntityKind) {
                var sResolvedDbKey = normalizeDbKey(sDbKey);
                var sResolvedEntityKind = normalizeEntityKind(sEntityKind);
                var sTabSessionId = resolveTabSessionId();
                if (!sResolvedDbKey || !sTabSessionId) {
                    return Promise.resolve(false);
                }
                return withStore("readwrite", function (st) {
                    st.delete(buildStoreKey(sTabSessionId, sResolvedEntityKind, sResolvedDbKey));
                    return true;
                });
            },
            clearCurrentTab: function () {
                return clearByTabSessionId(resolveTabSessionId());
            },
            clearByTabSessionId: clearByTabSessionId,
            cleanupStaleSessions: cleanupStaleSessions,
            resolveTabSessionId: resolveTabSessionId,
            describeStorageContract: function () {
                return {
                    sessionCache: "indexeddb_tab_session",
                    preferences: "persistent_local_storage",
                    personalization: "sap_standard_personalization"
                };
            }
        };
    }

    return { create: create };
});
