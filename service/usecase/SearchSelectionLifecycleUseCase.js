sap.ui.define([], function () {
    "use strict";

    function runSelectionChangeLifecycle(mArgs) {
        var fnExtractId = mArgs && mArgs.extractId;
        if (typeof fnExtractId !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_event_adapter" });
        }

        var sId = fnExtractId(mArgs && mArgs.event);
        var fnHydrateSelection = mArgs && mArgs.hydrateSelection;
        if (typeof fnHydrateSelection !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_hydration_adapter", id: sId || "" });
        }

        return Promise.resolve(fnHydrateSelection(sId)).then(function (oResult) {
            return { ok: true, reason: "selection_hydrated", id: sId || "", result: oResult || null };
        });
    }

    function runItemPressLifecycle(mArgs) {
        return runSelectionChangeLifecycle(mArgs).then(function (oSelectionResult) {
            if (!oSelectionResult || oSelectionResult.ok !== true) {
                return oSelectionResult;
            }

            var fnOpenDetail = mArgs && mArgs.openDetail;
            if (typeof fnOpenDetail !== "function") {
                return { ok: false, reason: "missing_open_adapter", id: oSelectionResult.id || "" };
            }

            return Promise.resolve(fnOpenDetail(oSelectionResult.id)).then(function (oOpenResult) {
                return {
                    ok: true,
                    reason: "item_press_applied",
                    id: oSelectionResult.id || "",
                    selection: oSelectionResult,
                    open: oOpenResult || null
                };
            });
        });
    }

    return {
        runSelectionChangeLifecycle: runSelectionChangeLifecycle,
        runItemPressLifecycle: runItemPressLifecycle
    };
});
