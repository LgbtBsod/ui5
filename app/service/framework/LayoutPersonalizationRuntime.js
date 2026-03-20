sap.ui.define([], function () {
    "use strict";

    // Persistent local preferences live here. Session-scoped runtime/search/detail
    // snapshots belong to BrowserCacheAdapter in IndexedDB and must not be merged
    // with personalization concerns.
    var STORAGE_KEY = "checklist_app_layout_personalization";

    function normalizeInfoCardLayout(aLayout) {
        return (Array.isArray(aLayout) ? aLayout : []).reduce(function (aResult, oEntry, iIndex) {
            var sKey = String(oEntry && oEntry.key || "").trim();
            if (!sKey || aResult.some(function (oItem) { return oItem.key === sKey; })) {
                return aResult;
            }
            aResult.push({ key: sKey, pinned: !!(oEntry && oEntry.pinned), order: Number.isFinite(oEntry && oEntry.order) ? Number(oEntry.order) : iIndex });
            return aResult;
        }, []);
    }

    function normalize(vRaw) {
        var oSource = vRaw && typeof vRaw === "object" ? vRaw : {};
        return {
            compactRows: !!oSource.compactRows,
            showHints: oSource.showHints !== false,
            infoCardLayout: normalizeInfoCardLayout(oSource.infoCardLayout)
        };
    }

    function readAll() {
        var sRaw = "";
        try {
            sRaw = window.localStorage.getItem(STORAGE_KEY) || "";
            return normalize(sRaw ? JSON.parse(sRaw) : {});
        } catch (oError) {
            return normalize({});
        }
    }

    function writeInfoCardLayout(aLayout) {
        var oData = readAll();
        oData.infoCardLayout = normalizeInfoCardLayout(aLayout);
        try {
            window.localStorage.setItem(STORAGE_KEY, JSON.stringify(oData));
        } catch (oError) {
            // Best-effort persistence only.
        }
        return oData.infoCardLayout;
    }

    return {
        readAll: readAll,
        writeInfoCardLayout: writeInfoCardLayout
    };
});
