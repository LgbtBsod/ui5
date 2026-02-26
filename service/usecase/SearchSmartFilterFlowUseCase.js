sap.ui.define([], function () {
    "use strict";

    function extractSmartFilterValue(vValue) {
        if (typeof vValue === "string") {
            return vValue;
        }
        if (Array.isArray(vValue) && vValue.length) {
            return extractSmartFilterValue(vValue[0]);
        }
        if (vValue && typeof vValue === "object") {
            if (typeof vValue.value !== "undefined") {
                return String(vValue.value || "");
            }
            if (typeof vValue.key !== "undefined") {
                return String(vValue.key || "");
            }
            if (Array.isArray(vValue.items) && vValue.items.length) {
                return extractSmartFilterValue(vValue.items[0]);
            }
            if (Array.isArray(vValue.ranges) && vValue.ranges.length) {
                var oRange = vValue.ranges[0] || {};
                return String(oRange.value1 || "");
            }
        }
        return "";
    }

    function resolveFilterPatch(mData) {
        var oData = mData || {};

        function pickFirstNonEmptyValue(aCandidates) {
            for (var iIndex = 0; iIndex < aCandidates.length; iIndex += 1) {
                var sCandidate = extractSmartFilterValue(aCandidates[iIndex]);
                if (String(sCandidate || "").trim()) {
                    return sCandidate;
                }
            }
            return "";
        }

        return {
            filterId: pickFirstNonEmptyValue([oData.checklist_id, oData.CHECKLIST_ID, oData.id, oData.ID]),
            filterLpc: pickFirstNonEmptyValue([oData.lpc, oData.LPC_KEY, oData.LPC])
        };
    }

    function syncStateFiltersFromSmartFilter(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        var oSmartFilterBar = mArgs && mArgs.smartFilterBar;
        if (!oStateModel || !oSmartFilterBar || typeof oSmartFilterBar.getFilterData !== "function") {
            return null;
        }

        var mPatch = resolveFilterPatch(oSmartFilterBar.getFilterData(true) || {});
        oStateModel.setProperty("/filterId", mPatch.filterId);
        oStateModel.setProperty("/filterLpc", mPatch.filterLpc);
        return mPatch;
    }

    function prepareRebindParams(mArgs) {
        var fnApplyRebindParams = mArgs && mArgs.applyRebindParams;
        if (typeof fnApplyRebindParams !== "function") {
            return mArgs && mArgs.bindingParams;
        }

        return fnApplyRebindParams({
            bindingParams: (mArgs && mArgs.bindingParams) || {},
            state: (mArgs && mArgs.state) || {},
            smartFilterData: (mArgs && mArgs.smartFilterData) || {},
            onDataReceived: mArgs && mArgs.onDataReceived
        });
    }

    return {
        extractSmartFilterValue: extractSmartFilterValue,
        resolveFilterPatch: resolveFilterPatch,
        syncStateFiltersFromSmartFilter: syncStateFiltersFromSmartFilter,
        prepareRebindParams: prepareRebindParams
    };
});
