sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/constants/LocationValueHelpConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts"
], function (StatePaths, Result, Effects, DetailStateAccess, UseCaseValue, LocationValueHelpConstants, ModelContracts, OperationSourceContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_INTENTS = OperationSourceContracts.DETAIL;
    var DETAIL_PATHS = LocationValueHelpConstants.DETAIL_PATHS;
    var DIALOG_ACTIONS = LocationValueHelpConstants.DIALOG_ACTIONS;
    var SESSION_CACHE_KEY = LocationValueHelpConstants.SESSION_CACHE_KEY;
    var VIEW_PATHS = LocationValueHelpConstants.VIEW_PATHS;

    function ValueHelpLocationUseCase() {
        var oLocalState = {
            _mSessionCache: {},
            _mInflightLoads: {}
        };

        return {
            execute: function (mInput, mCtx) {
                var sIntent = String((mInput && mInput.intent) || DETAIL_INTENTS.OPEN);
                var oUiState = mCtx && mCtx.uiState;
                var sCacheKey = resolveCacheKey(mCtx);
                var aViewItems = readViewCache(oUiState, sCacheKey);

                if (sIntent === DETAIL_INTENTS.TREE_SELECTION) {
                    var oEvent = mInput && mInput.event;
                    var oRowCtx = oEvent && oEvent.getParameter && oEvent.getParameter("rowContext");
                    var oRow = oRowCtx && oRowCtx.getObject ? oRowCtx.getObject() : null;
                    oUiState && oUiState.set && oUiState.set(VIEW_MODEL, VIEW_PATHS.SELECTION, oRow || null);
                    return Promise.resolve(Result.ok({ selected: !!oRow }, [Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, !!oRow)]));
                }

                if (sIntent === DETAIL_INTENTS.CONFIRM) {
                    var oSelected = oUiState && oUiState.get(VIEW_MODEL, VIEW_PATHS.SELECTION);
                    var aVisibleItems = (oUiState && oUiState.get(VIEW_MODEL, VIEW_PATHS.TREE)) || [];
                    var bHasValidSelection = hasVisibleSelection(aVisibleItems, oSelected);
                    if (!bHasValidSelection) {
                        oSelected = null;
                    }
                    return Promise.resolve(Result.ok({ selected: !!oSelected }, [
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_PATHS.LOCATION_NAME, (oSelected && oSelected.location_name) || ""),
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_PATHS.LOCATION_TEXT, (oSelected && (oSelected.location_text || oSelected.location_name)) || ""),
                        Effects.modelPatch(DETAIL_MODEL, DETAIL_PATHS.LOCATION_KEY, (oSelected && (oSelected.location_code || oSelected.location_id)) || ""),
                        Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, !!oSelected),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.SELECTION, null),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, false),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, ""),
                        Effects.dialog(LocationValueHelpConstants.DIALOG_ID, DIALOG_ACTIONS.CLOSE, {})
                    ]));
                }

                if (sIntent === DETAIL_INTENTS.OPEN) {
                    if (aViewItems.length) {
                        return Promise.resolve(Result.ok({ intent: sIntent, items: aViewItems, cacheKey: sCacheKey }, buildLoadedEffects(aViewItems, true, sCacheKey)));
                    }
                    return ensureItemsLoaded(oLocalState, mCtx).then(function (oLoaded) {
                        return Result.ok({ intent: sIntent, items: oLoaded.items, cacheKey: oLoaded.cacheKey }, buildLoadedEffects(oLoaded.items, true, oLoaded.cacheKey));
                    }).catch(function (oError) {
                        return Result.fail(oError, [
                            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, LocationValueHelpConstants.HINTS.NO_DATA),
                            Effects.dialog(LocationValueHelpConstants.DIALOG_ID, DIALOG_ACTIONS.OPEN, {})
                        ]);
                    });
                }

                if (sIntent !== DETAIL_INTENTS.SEARCH) {
                    return Promise.resolve(Result.ok({ intent: sIntent }, [
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.SELECTION, null),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, false),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, ""),
                        Effects.dialog(LocationValueHelpConstants.DIALOG_ID, sIntent, {})
                    ]));
                }

                var sQuery = String((mInput && mInput.value) || "").trim();
                if (aViewItems.length) {
                    var aFilteredLoaded = filterItems(aViewItems, sQuery);
                    return Promise.resolve(Result.ok({ items: aFilteredLoaded, cacheKey: sCacheKey }, [
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.CACHE_KEY, sCacheKey),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.TREE, aFilteredLoaded),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.SELECTION, null),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, false),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, aFilteredLoaded.length ? "" : LocationValueHelpConstants.HINTS.NO_DATA)
                    ]));
                }

                return ensureItemsLoaded(oLocalState, mCtx).then(function (oLoaded) {
                    var aFiltered = filterItems(oLoaded.items, sQuery);
                    return Result.ok({ items: aFiltered, cacheKey: oLoaded.cacheKey }, [
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.CACHE_KEY, oLoaded.cacheKey),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.TREE_SOURCE, oLoaded.items),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.TREE, aFiltered),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.LOADED, true),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.SELECTION, null),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, false),
                        Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, aFiltered.length ? "" : LocationValueHelpConstants.HINTS.NO_DATA)
                    ]);
                }).catch(function (oError) {
                    return Result.fail(oError);
                });
            }
        };
    }

    function resolveCacheKey(mCtx) {
        var sDateCheck = String(DetailStateAccess.resolveDateCheck(mCtx) || "").trim();
        return sDateCheck || LocationValueHelpConstants.DEFAULT_CACHE_KEY;
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
        return String((oUiState && oUiState.get && oUiState.get(VIEW_MODEL, VIEW_PATHS.CACHE_KEY)) || "").trim();
    }

    function readViewCache(oUiState, sCacheKey) {
        var bLoaded = !!(oUiState && oUiState.get && oUiState.get(VIEW_MODEL, VIEW_PATHS.LOADED));
        var sViewCacheKey = currentViewCacheKey(oUiState);
        var aCachedSource = (oUiState && oUiState.get && oUiState.get(VIEW_MODEL, VIEW_PATHS.TREE_SOURCE)) || [];
        var aCachedTree = (oUiState && oUiState.get && oUiState.get(VIEW_MODEL, VIEW_PATHS.TREE)) || [];
        if (!bLoaded || sViewCacheKey !== sCacheKey) {
            return [];
        }
        return safeCloneItems(Array.isArray(aCachedSource) && aCachedSource.length ? aCachedSource : aCachedTree);
    }

    function buildLoadedEffects(aItems, bOpen, sCacheKey) {
        var aSafeItems = safeCloneItems(aItems);
        var aEffects = [
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.CACHE_KEY, sCacheKey),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.TREE_SOURCE, aSafeItems),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.TREE, aSafeItems),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.LOADED, true),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.SELECTION, null),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HAS_SELECTION, false),
            Effects.modelPatch(VIEW_MODEL, VIEW_PATHS.HINT, aSafeItems.length ? "" : LocationValueHelpConstants.HINTS.NO_DATA)
        ];
        if (bOpen) {
            aEffects.push(Effects.dialog(LocationValueHelpConstants.DIALOG_ID, DIALOG_ACTIONS.OPEN, {}));
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
            return oLookup && oLookup.search({ query: "", limit: LocationValueHelpConstants.SEARCH_LIMIT, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
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

    function resolveSelectionKey(oItem) {
        if (!oItem || typeof oItem !== "object") {
            return "";
        }
        return String(oItem.location_id || oItem.location_code || oItem.location_name || "").trim();
    }

    function hasVisibleSelection(aItems, oSelected) {
        var sSelectedKey = resolveSelectionKey(oSelected);
        return !!sSelectedKey && (aItems || []).some(function (oItem) {
            return resolveSelectionKey(oItem) === sSelectedKey;
        });
    }

    return ValueHelpLocationUseCase;
});
