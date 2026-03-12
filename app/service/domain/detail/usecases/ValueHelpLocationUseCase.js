sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseResultUtils"
], function (StatePaths, UseCase, Result, Effects, DetailStateAccess, UseCaseResultUtils) {
    "use strict";

    function ValueHelpLocationUseCase() {
        UseCase.call(this, "ValueHelpLocationUseCase");
    }

    ValueHelpLocationUseCase.prototype = Object.create(UseCase.prototype);
    ValueHelpLocationUseCase.prototype.constructor = ValueHelpLocationUseCase;

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
        var oLookup = mCtx && mCtx.locationLookup;
        var oUiState = mCtx && mCtx.uiState;
        var bLoaded = !!(oUiState && oUiState.get && oUiState.get("view", "/locationVhLoaded"));
        var aCachedSource = (oUiState && oUiState.get && oUiState.get("view", "/locationVhTreeSource")) || [];
        var aCachedTree = (oUiState && oUiState.get && oUiState.get("view", "/locationVhTree")) || [];

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
            if (bLoaded) {
                var aLoadedItems = Array.isArray(aCachedSource) && aCachedSource.length ? aCachedSource : aCachedTree;
                return Promise.resolve(Result.ok({ intent: sIntent, items: aLoadedItems }, [
                    Effects.modelPatch("view", "/locationVhTree", aLoadedItems),
                    Effects.modelPatch("view", "/locationVhSelection", null),
                    Effects.modelPatch("view", "/locationVhHasSelection", false),
                    Effects.modelPatch("view", "/locationVhHint", aLoadedItems.length ? "" : "locationValueHelpNoDataHint"),
                    Effects.dialog("locationValueHelp", "open", {})
                ]));
            }
            return UseCaseResultUtils.callOrDefault(function () {
                return oLookup && oLookup.search({ query: "", limit: 200, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
            }, { items: [] }).then(function (oFound) {
                var aItems = (oFound && oFound.items) || [];
                return Result.ok({ intent: sIntent, items: aItems }, [
                    Effects.modelPatch("view", "/locationVhTreeSource", aItems),
                    Effects.modelPatch("view", "/locationVhTree", aItems),
                    Effects.modelPatch("view", "/locationVhLoaded", true),
                    Effects.modelPatch("view", "/locationVhSelection", null),
                    Effects.modelPatch("view", "/locationVhHasSelection", false),
                    Effects.modelPatch("view", "/locationVhHint", aItems.length ? "" : "locationValueHelpNoDataHint"),
                    Effects.dialog("locationValueHelp", "open", {})
                ]);
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
        if (bLoaded) {
            var aFiltered = filterItems(Array.isArray(aCachedSource) && aCachedSource.length ? aCachedSource : aCachedTree, sQuery);
            return Promise.resolve(Result.ok({ items: aFiltered }, [
                Effects.modelPatch("view", "/locationVhTree", aFiltered),
                Effects.modelPatch("view", "/locationVhHasSelection", false),
                Effects.modelPatch("view", "/locationVhHint", aFiltered.length ? "" : "locationValueHelpNoDataHint")
            ]));
        }
        return UseCaseResultUtils.callOrDefault(function () {
            return oLookup && oLookup.search({ query: sQuery, limit: 200, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
        }, { items: [] }).then(function (oFound) {
            var aItems = (oFound && oFound.items) || [];
            return Result.ok({ items: aItems }, [
                Effects.modelPatch("view", "/locationVhTreeSource", aItems),
                Effects.modelPatch("view", "/locationVhTree", aItems),
                Effects.modelPatch("view", "/locationVhLoaded", true),
                Effects.modelPatch("view", "/locationVhHasSelection", false),
                Effects.modelPatch("view", "/locationVhHint", aItems.length ? "" : "locationValueHelpNoDataHint")
            ]);
        }).catch(function (oError) {
            return Result.fail(oError);
        });
    };

    return ValueHelpLocationUseCase;
});
