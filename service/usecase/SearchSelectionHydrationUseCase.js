sap.ui.define([], function () {
    "use strict";

    function normalizeChecklist(vChecklist, sId) {
        if (vChecklist && typeof vChecklist === "object" && vChecklist.root && vChecklist.root.id) {
            return vChecklist;
        }
        return { root: { id: sId || "" } };
    }

    function runSelectionHydration(mArgs) {
        var sId = ((mArgs && mArgs.id) || "").trim();
        if (!sId) {
            return Promise.resolve({ ok: false, reason: "missing_id", checklist: null });
        }

        var oSelectedModel = mArgs && mArgs.selectedModel;
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oSelectedModel || typeof oSelectedModel.setData !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_selected_model", checklist: null, id: sId });
        }

        var fnLoad = mArgs && mArgs.loadChecklistById;
        if (typeof fnLoad !== "function") {
            var oFallbackNoLoader = normalizeChecklist(null, sId);
            oSelectedModel.setData(oFallbackNoLoader);
            if (oViewModel && typeof oViewModel.setProperty === "function") {
                oViewModel.setProperty("/hasSelection", true);
            }
            return Promise.resolve({ ok: false, reason: "missing_loader", checklist: oFallbackNoLoader, id: sId });
        }

        return Promise.resolve(fnLoad(sId)).then(function (oChecklist) {
            var oNormalized = normalizeChecklist(oChecklist, sId);
            oSelectedModel.setData(oNormalized);
            if (oViewModel && typeof oViewModel.setProperty === "function") {
                oViewModel.setProperty("/hasSelection", true);
            }
            return { ok: true, reason: "loaded", checklist: oNormalized, id: sId };
        }).catch(function (_e) {
            var oFallback = normalizeChecklist(null, sId);
            oSelectedModel.setData(oFallback);
            if (oViewModel && typeof oViewModel.setProperty === "function") {
                oViewModel.setProperty("/hasSelection", true);
            }
            return { ok: false, reason: "load_error", checklist: oFallback, id: sId };
        });
    }

    return {
        normalizeChecklist: normalizeChecklist,
        runSelectionHydration: runSelectionHydration
    };
});
