sap.ui.define([], function () {
    "use strict";

    function resolveSelectedId(mArgs) {
        var fnResolve = mArgs && mArgs.resolveSelectedId;
        if (typeof fnResolve !== "function") {
            return "";
        }
        return fnResolve() || "";
    }

    function clearSelection(mArgs) {
        var fnApplySelectedChecklist = mArgs && mArgs.applySelectedChecklist;
        if (typeof fnApplySelectedChecklist !== "function") {
            return false;
        }

        fnApplySelectedChecklist({});
        return true;
    }

    function runDeleteFlow(mArgs) {
        var sId = resolveSelectedId(mArgs);
        if (!sId) {
            return Promise.resolve({ ok: false, reason: "missing_id" });
        }

        var fnDeleteAndReload = mArgs && mArgs.deleteAndReload;
        if (typeof fnDeleteAndReload !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_delete_adapter", id: sId });
        }

        var fnRunWithLoading = typeof (mArgs && mArgs.runWithLoading) === "function"
            ? mArgs.runWithLoading
            : function (fnTask) { return fnTask(); };
        var fnApplyRows = mArgs && mArgs.applyRows;
        var fnRebind = mArgs && mArgs.rebind;
        var fnReload = mArgs && mArgs.reloadSelectionState;

        return Promise.resolve(fnRunWithLoading(function () {
            return fnDeleteAndReload(sId);
        })).then(function (aRows) {
            if (typeof fnApplyRows === "function") {
                fnApplyRows(Array.isArray(aRows) ? aRows : []);
            }
            if (typeof fnRebind === "function") {
                fnRebind();
            }
            clearSelection({ applySelectedChecklist: mArgs && mArgs.applySelectedChecklist });
            if (typeof fnReload === "function") {
                fnReload();
            }
            return { ok: true, id: sId, rows: Array.isArray(aRows) ? aRows : [] };
        }).catch(function (oError) {
            return { ok: false, reason: "delete_error", id: sId, error: oError };
        });
    }

    return {
        resolveSelectedId: resolveSelectedId,
        clearSelection: clearSelection,
        runDeleteFlow: runDeleteFlow
    };
});
