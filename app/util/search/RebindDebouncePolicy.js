sap.ui.define([], function () {
    "use strict";

    function rebindOrFallback(mArgs) {
        if (!mArgs.enabled) {
            if (typeof mArgs.onSkipped === "function") { mArgs.onSkipped("smart_disabled"); }
            return;
        }
        var oSmartTable = mArgs.smartTable;
        if (oSmartTable && oSmartTable.rebindTable) { oSmartTable.rebindTable(); return; }
        if (typeof mArgs.onSkipped === "function") { mArgs.onSkipped("smart_table_unavailable"); }
    }

    function createDebouncedRebind(fnRebind, iDelayMs) {
        var iTimer = null;
        return function () {
            var aArgs = arguments;
            clearTimeout(iTimer);
            iTimer = setTimeout(function () { fnRebind.apply(null, aArgs); }, Math.max(0, Number(iDelayMs) || 0));
        };
    }

    return {
        rebindOrFallback: rebindOrFallback,
        createDebouncedRebind: createDebouncedRebind
    };
});
