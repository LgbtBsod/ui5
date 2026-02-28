sap.ui.define([], function () {
    "use strict";

    var _cache = {};

    function _parseDateMs(v) {
        var s = String(v || "");
        if (/^\/Date\(/.test(s)) {
            var n = Number(s.slice(6).split(")/")[0].split("+")[0].split("-")[0]);
            return Number.isFinite(n) ? n : 0;
        }
        var t = Date.parse(s);
        return Number.isFinite(t) ? t : 0;
    }

    return {
        checkCacheOrReload: function (rootKey, gatewayClient, objectLoader, runtimeSettings) {
            var oCached = _cache[rootKey] || null;
            if (!oCached) {
                return objectLoader.openChecklist(rootKey).then(function (oFresh) {
                    _cache[rootKey] = oFresh;
                    return oFresh;
                });
            }

            return gatewayClient.readEntity("LastChangeSet", "RootKey='" + rootKey + "'", {}).then(function (oLast) {
                var iSrv = _parseDateMs((oLast || {}).AggChangedOn);
                var iCli = _parseDateMs(oCached._aggChangedOn);
                var iTol = Number((runtimeSettings || {}).CacheToleranceMs || 5500);
                if (Math.abs(iSrv - iCli) <= iTol) {
                    return oCached;
                }
                return objectLoader.openChecklist(rootKey).then(function (oFresh) {
                    _cache[rootKey] = oFresh;
                    return oFresh;
                });
            });
        },

        applySnapshotToViewModel: function (oViewModel, snapshot) {
            if (snapshot && oViewModel && oViewModel.setProperty) {
                oViewModel.setProperty("/snapshot", snapshot);
            }
        },

        overwriteCache: function (rootKey, snapshot) { _cache[rootKey] = snapshot; },

        updateCacheAfterAutosave: function (rootKey, changes, newAgg) {
            _cache[rootKey] = Object.assign({}, _cache[rootKey] || {}, {
                _lastChanges: changes || [],
                _aggChangedOn: newAgg || ((_cache[rootKey] || {})._aggChangedOn)
            });
        },

        updateCacheAfterSave: function (rootKey, fullSnapshot, newAgg) {
            _cache[rootKey] = Object.assign({}, fullSnapshot || {}, { _aggChangedOn: newAgg || null });
        }
    };
});
