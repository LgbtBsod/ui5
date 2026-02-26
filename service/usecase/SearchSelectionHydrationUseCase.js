sap.ui.define([], function () {
    "use strict";

    function normalizeChecklist(vChecklist, sId) {
        if (vChecklist && typeof vChecklist === "object" && vChecklist.root && vChecklist.root.id) {
            return vChecklist;
        }
        return { root: { id: sId || "" } };
    }

    function setSelectionFlag(oViewModel, bHasSelection) {
        if (oViewModel && typeof oViewModel.setProperty === "function") {
            oViewModel.setProperty("/hasSelection", !!bHasSelection);
        }
    }

    function runSelectionHydration(mArgs) {
        var sId = ((mArgs && mArgs.id) || "").trim();
        var oViewModel = mArgs && mArgs.viewModel;
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!sId) {
            if (oSelectedModel && typeof oSelectedModel.setData === "function") {
                oSelectedModel.setData({});
            }
            setSelectionFlag(oViewModel, false);
            return Promise.resolve({ ok: false, reason: "missing_id", checklist: null });
        }

        if (!oSelectedModel || typeof oSelectedModel.setData !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_selected_model", checklist: null, id: sId });
        }

        var fnLoad = mArgs && mArgs.loadChecklistById;
        if (typeof fnLoad !== "function") {
            var oFallbackNoLoader = normalizeChecklist(null, sId);
            oSelectedModel.setData(oFallbackNoLoader);
            setSelectionFlag(oViewModel, true);
            return Promise.resolve({ ok: false, reason: "missing_loader", checklist: oFallbackNoLoader, id: sId });
        }

        return Promise.resolve(fnLoad(sId)).then(function (oChecklist) {
            var oNormalized = normalizeChecklist(oChecklist, sId);
            oSelectedModel.setData(oNormalized);
            setSelectionFlag(oViewModel, true);
            return { ok: true, reason: "loaded", checklist: oNormalized, id: sId };
        }).catch(function (_e) {
            var oFallback = normalizeChecklist(null, sId);
            oSelectedModel.setData(oFallback);
            setSelectionFlag(oViewModel, true);
            return { ok: false, reason: "load_error", checklist: oFallback, id: sId };
        });
    }

    return {
        normalizeChecklist: normalizeChecklist,
        setSelectionFlag: setSelectionFlag,
        runSelectionHydration: runSelectionHydration
    };
});
