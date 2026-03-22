sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Result, Effects, DetailStateAccess, StatePaths, UseCaseValue, WorkflowContracts, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_MODEL = ModelContracts.MODELS.DETAIL;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function PersonSuggestUseCase() {
        return {
            execute: execute
        };
    }

function selectedItemPayload(oItem) {
        if (!oItem || !oItem.getBindingContext) { return null; }
        var oCtx = oItem.getBindingContext(VIEW_MODEL);
        return oCtx && oCtx.getObject ? oCtx.getObject() : null;
    }

    function resolveMode(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return WorkflowContracts.normalizeEditMode(oUiState && oUiState.get(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
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
    function requestVersionPathForTarget(sTarget) {
        return String(sTarget || "").toLowerCase() === "observed" ? "/observedSuggestRequestVersion" : "/observerSuggestRequestVersion";
    }

    function advanceRequestVersion(oUiState, sTarget) {
        var sPath = requestVersionPathForTarget(sTarget);
        var iNext = Number((oUiState && oUiState.get && oUiState.get(VIEW_MODEL, sPath)) || 0) + 1;
        oUiState && oUiState.set && oUiState.set(VIEW_MODEL, sPath, iNext);
        return iNext;
    }

    function isCurrentRequestVersion(oUiState, sTarget, iVersion) {
        return Number((oUiState && oUiState.get && oUiState.get(VIEW_MODEL, requestVersionPathForTarget(sTarget))) || 0) === Number(iVersion || 0);
    }

    function execute(mInput, mCtx) {
        var sIntent = String((mInput && mInput.intent) || "suggest");
        var oUiState = mCtx && mCtx.uiState;
        var sMode = resolveMode(mCtx);
        var sTarget = String((mInput && mInput.target) || "observer");
        var sPath = suggestionsPathForTarget(sTarget);
        var sInputPath = inputPathForTarget(sTarget);
        var sPrefix = personPrefix(sTarget);

        if (sMode !== WorkflowContracts.EDIT_MODES.EDIT) {
            advanceRequestVersion(oUiState, sTarget);
            return Promise.resolve(Result.ok({ skipped: true, items: [] }, [
                Effects.modelPatch(VIEW_MODEL, sPath, []),
                Effects.modelPatch(VIEW_MODEL, "/personSuggestHint", "")
            ]));
        }

        if (sIntent === "selected") {
            advanceRequestVersion(oUiState, sTarget);
            var oItem = selectedItemPayload(mInput && mInput.item);
            if (!oItem) { return Promise.resolve(Result.ok({ selected: false }, [])); }
            var aEffects = [
                Effects.modelPatch(VIEW_MODEL, sInputPath, oItem.fullName || ""),
                Effects.modelPatch(VIEW_MODEL, sPath, []),
                Effects.modelPatch(VIEW_MODEL, "/personSuggestHint", ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_FULLNAME", oItem.fullName || ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_PERNER", oItem.perner || ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_POSITION", oItem.position || ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_ORGUNIT", oItem.orgUnit || ""),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, true)
            ];
            return Promise.resolve(Result.ok({ selected: true }, aEffects));
        }
        if (sIntent === "manualChange") {
            advanceRequestVersion(oUiState, sTarget);
            var sValue = String((mInput && mInput.value) || "");
            return Promise.resolve(Result.ok({ manualChange: true }, [
                Effects.modelPatch(VIEW_MODEL, sInputPath, sValue),
                Effects.modelPatch(VIEW_MODEL, sPath, []),
                Effects.modelPatch(VIEW_MODEL, "/personSuggestHint", ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_FULLNAME", sValue),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_PERNER", ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_POSITION", ""),
                Effects.modelPatch(DETAIL_MODEL, "/current/basic/" + sPrefix + "_ORGUNIT", ""),
                Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, true)
            ]));
        }

        var sTerm = String((mInput && mInput.term) || "").trim();
        var oSuggest = mCtx && mCtx.personSuggest;

        if (sTerm.length < 2) {
            advanceRequestVersion(oUiState, sTarget);
            return Promise.resolve(Result.ok({ items: [] }, [
                Effects.modelPatch(VIEW_MODEL, sPath, []),
                Effects.modelPatch(VIEW_MODEL, "/personSuggestHint", "")
            ]));
        }

        var iRequestVersion = advanceRequestVersion(oUiState, sTarget);

        return UseCaseValue.callOrDefault(function () {
            return oSuggest && oSuggest.suggest({ query: sTerm, limit: 12, dateCheck: DetailStateAccess.resolveDateCheck(mCtx) });
        }, { items: [] }).then(function (oRes) {
            var aItems = (oRes && oRes.items) || [];
            if (!isCurrentRequestVersion(oUiState, sTarget, iRequestVersion)) {
                return Result.ok({ stale: true, items: [] }, []);
            }
            aItems = aItems.map(function (oItem) {
                return Object.assign({}, oItem || {}, {
                    fullName: String((oItem && oItem.fullName) || "").trim(),
                    perner: String((oItem && oItem.perner) || "").trim(),
                    position: String((oItem && oItem.position) || "").trim(),
                    orgUnit: String((oItem && oItem.orgUnit) || "").trim()
                });
            });
            return Result.ok({ items: aItems }, [
                Effects.modelPatch(VIEW_MODEL, sPath, aItems),
                Effects.modelPatch(VIEW_MODEL, "/personSuggestHint", aItems.length ? "" : "personSuggestNoDataHint")
            ]);
        }).catch(function (oError) {
            if (!isCurrentRequestVersion(oUiState, sTarget, iRequestVersion)) {
                return Result.ok({ stale: true, items: [] }, []);
            }
            return Result.fail(oError);
        });
    }

    return PersonSuggestUseCase;
});
