sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue"
], function (StatePaths, UseCase, Result, Effects, DetailStateAccess, UseCaseValue) {
    "use strict";

    var SESSION_CACHE_KEY = "pcct_detail_location_vh_cache_v1";

    function ValueHelpLocationUseCase() {
        UseCase.call(this, "ValueHelpLocationUseCase");
        this._mSessionCache = {};
        this._mInflightLoads = {};
    }

    ValueHelpLocationUseCase.prototype = Object.create(UseCase.prototype);
    ValueHelpLocationUseCase.prototype.constructor = ValueHelpLocationUseCase;

    function resolveCacheKey(mCtx) {
        var sDateCheck = String(DetailStateAccess.resolveDateCheck(mCtx) || "").trim();
        return sDateCheck || "__default__";
    }

    function safeCloneItems(aItems) {
        return Array.isArray(aItems) ? aItems.slice() : [];
    }

    function readSessionCacheMap() {
        var sRaw;
        if (typeof window === "undefined" || !window.sessionStorage) {
            return {};
        }
        try {
            sRaw = window.sessionStorage.getItem(SESSION_CACHE_KEY) || "";
            return sRaw ? (JSON.parse(sRaw) || {}) : {};
        } catch (_storageReadError) {
            return {};
        }
    }

    function writeSessionCacheMap(mCache) {
        if (typeof window === "undefined" || !window.sessionStorage) {
            return;
        }
        try {
            window.sessionStorage.setItem(SESSION_CACHE_KEY, JSON.stringify(mCache || {}));
        } catch (_storageWriteError) {
            return;
        }
    }

    function readCachedItems(oUseCase, sCacheKey) {
        var mStored;
        var aItems = oUseCase && oUseCase._mSessionCache && oUseCase._mSessionCache[sCacheKey];
        if (Array.isArray(aItems) && aItems.length) {
            return safeCloneItems(aItems);
        }
        mStored = readSessionCacheMap();
        aItems = mStored[sCacheKey];
        if (!Array.isArray(aItems) || !aItems.length) {
            return [];
        }
        if (oUseCase && oUseCase._mSessionCache) {
            oUseCase._mSessionCache[sCacheKey] = safeCloneItems(aItems);
        }
        return safeCloneItems(aItems);
    }

    function writeCachedItems(oUseCase, sCacheKey, aItems) {
        var mStored = readSessionCacheMap();
        var aSafeItems = safeCloneItems(aItems);
        if (oUseCase && oUseCase._mSessionCache) {
            oUseCase._mSessionCache[sCacheKey] = aSafeItems;
        }
        mStored[sCacheKey] = aSafeItems;
        writeSessionCacheMap(mStored);
        return safeCloneItems(aSafeItems);
    }

    function currentViewCacheKey(oUiState) {
        return String((oUiState && oUiState.get && oUiState.get("view", "/locationVhCacheKey")) || "").trim();
    }

    function readViewCache(oUiState, sCacheKey) {
        var bLoaded = !!(oUiState && oUiState.get && oUiState.get("view", "/locationVhLoaded"));
        var sViewCacheKey = currentViewCacheKey(oUiState);
        var aCachedSource = (oUiState && oUiState.get && oUiState.get("view", "/locationVhTreeSource")) || [];
        var aCachedTree = (oUiState && oUiState.get && oUiState.get("view", "/locationVhTree")) || [];
        if (!bLoaded || sViewCacheKey !== sCacheKey) {
            return [];
        }
        return safeCloneItems(Array.isArray(aCachedSource) && aCachedSource.length ? aCachedSource : aCachedTree);
    }

    function buildLoadedEffects(aItems, bOpen, sCacheKey) {
        var aSafeItems = safeCloneItems(aItems);
        var aEffects = [
            Effects.modelPatch("view", "/locationVhCacheKey", sCacheKey),
            Effects.modelPatch("view", "/locationVhTreeSource", aSafeItems),
            Effects.modelPatch("view", "/locationVhTree", aSafeItems),
            Effects.modelPatch("view", "/locationVhLoaded", true),
            Effects.modelPatch("view", "/locationVhSelection", null),
            Effects.modelPatch("view", "/locationVhHasSelection", false),
            Effects.modelPatch("view", "/locationVhHint", aSafeItems.length ? "" : "locationValueHelpNoDataHint")
        ];
        if (bOpen) {
            aEffects.push(Effects.dialog("locationValueHelp", "open", {}));
        }
        return aEffects;
    }

    function ensureItemsLoaded(oUseCase, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLookup = mCtx && mCtx.locationLookup;
        var sCacheKey = resolveCacheKey(mCtx);
        var aViewItems = readViewCache(oUiState, sCacheKey);
        var aSessionItems;
        var pLoad;
        if (aViewItems.length) {
            return Promise.resolve({
                items: aViewItems,
                cacheKey: sCacheKey,
                fromCache: true
            });
        }
        aSessionItems = readCachedItems(oUseCase, sCacheKey);
        if (aSessionItems.length) {
            return Promise.resolve({
                items: aSessionItems,
                cacheKey: sCacheKey,
                fromCache: true
            });
        }
        if (oUseCase && oUseCase._mInflightLoads && oUseCase._mInflightLoads[sCacheKey]) {
            return oUseCase._mInflightLoads[sCacheKey];
        }
        pLoad = UseCaseValue.callOrDefault(function () {
            return oLookup && oLookup.search({ query: "", limit: 200, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
        }, { items: [] }).then(function (oFound) {
            var aItems = writeCachedItems(oUseCase, sCacheKey, (oFound && oFound.items) || []);
            return {
                items: aItems,
                cacheKey: sCacheKey,
                fromCache: false
            };
        }).finally(function () {
            if (oUseCase && oUseCase._mInflightLoads) {
                delete oUseCase._mInflightLoads[sCacheKey];
            }
        });
        if (oUseCase && oUseCase._mInflightLoads) {
            oUseCase._mInflightLoads[sCacheKey] = pLoad;
        }
        return pLoad;
    }

    function normalize(vValue) {
        return String(vValue || "").toLowerCase();
    }

    function filterItems(aItems, sQuery) {
        var sNeedle = normalize(sQuery).trim();
        if (!sNeedle) {
            return (aItems || []).slice();
        }
        return (aItems || []).filter(function (oItem) {
            return normalize(oItem && oItem.location_name).indexOf(sNeedle) >= 0
                || normalize(oItem && oItem.location_code).indexOf(sNeedle) >= 0
                || normalize(oItem && oItem.location_id).indexOf(sNeedle) >= 0;
        });
    }

    ValueHelpLocationUseCase.prototype.execute = function (mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "open");
        var oUiState = mCtx && mCtx.uiState;
        var sCacheKey = resolveCacheKey(mCtx);
        var aViewItems = readViewCache(oUiState, sCacheKey);

        if (sIntent === "treeSelection") {
            var oEvent = mInput && mInput.event;
            var oRowCtx = oEvent && oEvent.getParameter && oEvent.getParameter("rowContext");
            var oRow = oRowCtx && oRowCtx.getObject ? oRowCtx.getObject() : null;
            oUiState && oUiState.set("view", "/locationVhSelection", oRow || null);
            return Promise.resolve(Result.ok({ selected: !!oRow }, [Effects.modelPatch("view", "/locationVhHasSelection", !!oRow)]));
        }

        if (sIntent === "confirm") {
            var oSelected = oUiState && oUiState.get("view", "/locationVhSelection");
            return Promise.resolve(Result.ok({ selected: !!oSelected }, [
                Effects.modelPatch("selected", "/basic/LOCATION_NAME", (oSelected && oSelected.location_name) || ""),
                Effects.modelPatch("selected", "/basic/LOCATION_TEXT", (oSelected && (oSelected.location_text || oSelected.location_name)) || ""),
                Effects.modelPatch("selected", "/basic/LOCATION_KEY", (oSelected && (oSelected.location_code || oSelected.location_id)) || ""),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, !!oSelected),
                Effects.modelPatch("view", "/locationVhHasSelection", false),
                Effects.dialog("locationValueHelp", "close", {})
            ]));
        }

        if (sIntent === "open") {
            if (aViewItems.length) {
                return Promise.resolve(Result.ok({ intent: sIntent, items: aViewItems, cacheKey: sCacheKey }, buildLoadedEffects(aViewItems, true, sCacheKey)));
            }
            return ensureItemsLoaded(this, mCtx).then(function (oLoaded) {
                return Result.ok({ intent: sIntent, items: oLoaded.items, cacheKey: oLoaded.cacheKey }, buildLoadedEffects(oLoaded.items, true, oLoaded.cacheKey));
            }).catch(function (oError) {
                return Result.fail(oError, [
                    Effects.modelPatch("view", "/locationVhHint", "locationValueHelpNoDataHint"),
                    Effects.dialog("locationValueHelp", "open", {})
                ]);
            });
        }

        if (sIntent !== "search") {
            return Promise.resolve(Result.ok({ intent: sIntent }, [
                Effects.modelPatch("view", "/locationVhHint", ""),
                Effects.dialog("locationValueHelp", sIntent, {})
            ]));
        }

        var sQuery = String((mInput && mInput.value) || "").trim();
        if (aViewItems.length) {
            var aFilteredLoaded = filterItems(aViewItems, sQuery);
            return Promise.resolve(Result.ok({ items: aFilteredLoaded, cacheKey: sCacheKey }, [
                Effects.modelPatch("view", "/locationVhCacheKey", sCacheKey),
                Effects.modelPatch("view", "/locationVhTree", aFilteredLoaded),
                Effects.modelPatch("view", "/locationVhHasSelection", false),
                Effects.modelPatch("view", "/locationVhHint", aFilteredLoaded.length ? "" : "locationValueHelpNoDataHint")
            ]));
        }
        return ensureItemsLoaded(this, mCtx).then(function (oLoaded) {
            var aFiltered = filterItems(oLoaded.items, sQuery);
            return Result.ok({ items: aFiltered, cacheKey: oLoaded.cacheKey }, [
                Effects.modelPatch("view", "/locationVhCacheKey", oLoaded.cacheKey),
                Effects.modelPatch("view", "/locationVhTreeSource", oLoaded.items),
                Effects.modelPatch("view", "/locationVhTree", aFiltered),
                Effects.modelPatch("view", "/locationVhLoaded", true),
                Effects.modelPatch("view", "/locationVhHasSelection", false),
                Effects.modelPatch("view", "/locationVhHint", aFiltered.length ? "" : "locationValueHelpNoDataHint")
            ]);
        }).catch(function (oError) {
            return Result.fail(oError);
        });
    };

    return ValueHelpLocationUseCase;
});
