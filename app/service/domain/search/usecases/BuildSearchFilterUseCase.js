sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/util/search/SearchFilterBuilder"
], function (UseCase, Result, Effects, StatePaths, SearchFilterBuilder) {
    "use strict";

    function BuildSearchFilterUseCase() {
        UseCase.call(this, "BuildSearchFilterUseCase");
    }

    BuildSearchFilterUseCase.prototype = Object.create(UseCase.prototype);
    BuildSearchFilterUseCase.prototype.constructor = BuildSearchFilterUseCase;

    BuildSearchFilterUseCase.prototype.execute = function (mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sChecks = String((mInput && mInput.intent === "checksSegment" && mInput.key) || (oUiState && oUiState.get("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS_CHECKS)) || "ALL").toUpperCase();
        var sBarriers = String((mInput && mInput.intent === "barriersSegment" && mInput.key) || (oUiState && oUiState.get("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS_BARRIERS)) || "ALL").toUpperCase();

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
            Effects.modelMerge("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS, {
                checksFailSegment: sChecks,
                barriersFailSegment: sBarriers
            }),
            Effects.modelPatch("view", "/filterHintVisible", false)
        ]));
    };

    return BuildSearchFilterUseCase;
});
