sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/runtime/SearchBootstrapRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/runtime/SearchExecutionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/runtime/SearchPresentationRuntime"
], function (
    SearchBootstrapRuntime,
    SearchExecutionRuntime,
    SearchSelectionRuntime,
    SearchPresentationRuntime
) {
    "use strict";

    function SearchFacade(mDeps) {
        var d = mDeps || {};
        this._runtime = {
            bootstrap: d.bootstrapRuntime || new SearchBootstrapRuntime(d),
            execution: d.executionRuntime || new SearchExecutionRuntime(d),
            selection: d.selectionRuntime || new SearchSelectionRuntime(d),
            presentation: d.presentationRuntime || new SearchPresentationRuntime(d)
        };
    }

    SearchFacade.prototype.bootstrap = function (i, c) { return this._runtime.bootstrap.bootstrap(i, c); };
    SearchFacade.prototype.buildFilter = function (i, c) { return this._runtime.execution.buildFilter(i, c); };
    SearchFacade.prototype.executeSearch = function (i, c) { return this._runtime.execution.executeSearch(i, c); };
    SearchFacade.prototype.rebind = function (i, c) { return this._runtime.execution.rebind(i, c); };
    SearchFacade.prototype.selectRow = function (i, c) { return this._runtime.selection.selectRow(i, c); };
    SearchFacade.prototype.selectionChanged = function (i, c) { return this._runtime.selection.selectionChanged(i, c); };
    SearchFacade.prototype.exportFlow = function (i, c) { return this._runtime.presentation.exportFlow(i, c); };
    SearchFacade.prototype.analytics = function (i, c) { return this._runtime.presentation.analytics(i, c); };
    SearchFacade.prototype.applyRebindPolicy = function (i, c) { return this._runtime.execution.applyRebindPolicy(i, c); };

    return SearchFacade;
});
