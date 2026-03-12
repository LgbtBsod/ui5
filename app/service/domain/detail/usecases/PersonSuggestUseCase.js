sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseResultUtils"
], function (UseCase, Result, Effects, DetailStateAccess, StatePaths, UseCaseResultUtils) {
    "use strict";

    function PersonSuggestUseCase() {
        UseCase.call(this, "PersonSuggestUseCase");
    }

    PersonSuggestUseCase.prototype = Object.create(UseCase.prototype);
    PersonSuggestUseCase.prototype.constructor = PersonSuggestUseCase;

    function selectedItemPayload(oItem) {
        if (!oItem || !oItem.getBindingContext) { return null; }
        var oCtx = oItem.getBindingContext("view");
        return oCtx && oCtx.getObject ? oCtx.getObject() : null;
    }

    function resolveMode(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((oUiState && oUiState.get("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE)) || "READ").toUpperCase();
    }
    function inputPathForTarget(sTarget) {
        return String(sTarget || "").toLowerCase() === "observed" ? "/observedInputValue" : "/observerInputValue";
    }
    function suggestionsPathForTarget(sTarget) {
        return String(sTarget || "").toLowerCase() === "observed" ? "/observedSuggestions" : "/observerSuggestions";
    }
    function personPrefix(sTarget) {
        return String(sTarget || "").toLowerCase() === "observed" ? "OBSERVED" : "OBSERVER";
    }

    PersonSuggestUseCase.prototype.execute = function (mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "suggest");
        var oUiState = mCtx && mCtx.uiState;
        var sMode = resolveMode(mCtx);
        var sTarget = String((mInput && mInput.target) || "observer");
        var sPath = suggestionsPathForTarget(sTarget);
        var sInputPath = inputPathForTarget(sTarget);
        var sPrefix = personPrefix(sTarget);

        if (sMode !== "EDIT") {
            return Promise.resolve(Result.ok({ skipped: true, items: [] }, [
                Effects.modelPatch("view", sPath, []),
                Effects.modelPatch("view", "/personSuggestHint", "")
            ]));
        }

        if (sIntent === "selected") {
            var oItem = selectedItemPayload(mInput && mInput.item);
            if (!oItem) { return Promise.resolve(Result.ok({ selected: false }, [])); }
            var aEffects = [
                Effects.modelPatch("view", sInputPath, oItem.fullName || ""),
                Effects.modelPatch("view", sPath, []),
                Effects.modelPatch("view", "/personSuggestHint", ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_FULLNAME", oItem.fullName || ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_PERNER", oItem.perner || ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_POSITION", oItem.position || ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_ORGUNIT", oItem.orgUnit || ""),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, true)
            ];
            return Promise.resolve(Result.ok({ selected: true }, aEffects));
        }
        if (sIntent === "manualChange") {
            var sValue = String((mInput && mInput.value) || "");
            return Promise.resolve(Result.ok({ manualChange: true }, [
                Effects.modelPatch("view", sInputPath, sValue),
                Effects.modelPatch("view", sPath, []),
                Effects.modelPatch("view", "/personSuggestHint", ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_FULLNAME", sValue),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_PERNER", ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_POSITION", ""),
                Effects.modelPatch("selected", "/basic/" + sPrefix + "_ORGUNIT", ""),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, true)
            ]));
        }

        var sTerm = String((mInput && mInput.term) || "").trim();
        var oSuggest = mCtx && mCtx.personSuggest;

        if (sTerm.length < 2) {
            return Promise.resolve(Result.ok({ items: [] }, [
                Effects.modelPatch("view", sPath, []),
                Effects.modelPatch("view", "/personSuggestHint", "")
            ]));
        }

        return UseCaseResultUtils.callOrDefault(function () {
            return oSuggest && oSuggest.suggest({ query: sTerm, limit: 12, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
        }, { items: [] }).then(function (oRes) {
            var aItems = (oRes && oRes.items) || [];
            aItems = aItems.map(function (oItem) {
                return Object.assign({}, oItem || {}, {
                    fullName: String((oItem && oItem.fullName) || "").trim(),
                    perner: String((oItem && oItem.perner) || "").trim(),
                    position: String((oItem && oItem.position) || "").trim(),
                    orgUnit: String((oItem && oItem.orgUnit) || "").trim()
                });
            });
            return Result.ok({ items: aItems }, [
                Effects.modelPatch("view", sPath, aItems),
                Effects.modelPatch("view", "/personSuggestHint", aItems.length ? "" : "personSuggestNoDataHint")
            ]);
        }).catch(function (oError) {
            return Result.fail(oError);
        });
    };

    return PersonSuggestUseCase;
});
