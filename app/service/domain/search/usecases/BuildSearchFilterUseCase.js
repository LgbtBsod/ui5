sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchFilterBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts"
], function (Result, Effects, StatePaths, SearchFilterBuilder, ModelContracts, SearchRuntimeConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var SEARCH_SEGMENTS = SearchRuntimeConstants.SEARCH_SEGMENTS;

    function BuildSearchFilterUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

function execute(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sChecks = String((mInput && mInput.intent === "checksSegment" && mInput.key) || (oUiState && oUiState.get(STATE_MODEL, StatePaths.SEARCH_CHECKS_FAIL_SEGMENT)) || SEARCH_SEGMENTS.ALL).toUpperCase();
        var sBarriers = String((mInput && mInput.intent === "barriersSegment" && mInput.key) || (oUiState && oUiState.get(STATE_MODEL, StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT)) || SEARCH_SEGMENTS.ALL).toUpperCase();

        var fnBuildFilters = SearchFilterBuilder && SearchFilterBuilder.buildFilters;
        var aFilters = (typeof fnBuildFilters === "function")
            ? fnBuildFilters({
                checksSegment: sChecks,
                barriersSegment: sBarriers
            })
            : [
                SearchFilterBuilder && SearchFilterBuilder.buildFailSegmentFilter ? SearchFilterBuilder.buildFailSegmentFilter(sChecks) : null,
                SearchFilterBuilder && SearchFilterBuilder.buildBarrierFailSegmentFilter ? SearchFilterBuilder.buildBarrierFailSegmentFilter(sBarriers) : null
            ].filter(Boolean);

        return Promise.resolve(Result.ok({ filters: aFilters || [] }, [
            Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, sChecks),
            Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, sBarriers),
            Effects.modelPatch(VIEW_MODEL, "/filterHintVisible", false)
        ]));
    }

    return BuildSearchFilterUseCase;
});
