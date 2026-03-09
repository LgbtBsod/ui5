sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/BootstrapSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/BuildSearchFilterUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExecuteSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/RebindSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectRowUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectionChangedUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/ExportFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/AnalyticsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ApplyRebindPolicyUseCase"
], function (
    BootstrapSearchUseCase,
    BuildSearchFilterUseCase,
    ExecuteSearchUseCase,
    RebindSearchUseCase,
    SelectRowUseCase,
    SelectionChangedUseCase,
    ExportFacade,
    AnalyticsUseCase,
    ApplyRebindPolicyUseCase
) {
    "use strict";

    function SearchFacade(mDeps) {
        var d = mDeps || {};
        this._exportFacade = d.exportFacade || new ExportFacade({ exportUseCase: d.exportUseCase });
        this._uc = {
            bootstrap: d.bootstrapUseCase || new BootstrapSearchUseCase(),
            buildFilter: d.buildFilterUseCase || new BuildSearchFilterUseCase(),
            execute: d.executeUseCase || new ExecuteSearchUseCase(),
            rebind: d.rebindUseCase || new RebindSearchUseCase(),
            selectRow: d.selectRowUseCase || new SelectRowUseCase(),
            selectionChanged: d.selectionChangedUseCase || new SelectionChangedUseCase(),
            analytics: d.analyticsUseCase || new AnalyticsUseCase(),
            applyRebindPolicy: d.applyRebindPolicyUseCase || new ApplyRebindPolicyUseCase()
        };
    }

    Object.keys(SearchFacade.prototype).forEach(function () {});
    SearchFacade.prototype.bootstrap = function (i, c) { return this._uc.bootstrap.execute(i || {}, c || {}); };
    SearchFacade.prototype.buildFilter = function (i, c) { return this._uc.buildFilter.execute(i || {}, c || {}); };
    SearchFacade.prototype.executeSearch = function (i, c) { return this._uc.execute.execute(i || {}, c || {}); };
    SearchFacade.prototype.rebind = function (i, c) { return this._uc.rebind.execute(i || {}, c || {}); };
    SearchFacade.prototype.selectRow = function (i, c) { return this._uc.selectRow.execute(i || {}, c || {}); };
    SearchFacade.prototype.selectionChanged = function (i, c) { return this._uc.selectionChanged.execute(i || {}, c || {}); };
    SearchFacade.prototype.exportFlow = function (i, c) { return this._exportFacade.exportFlow(i || {}, c || {}); };
    SearchFacade.prototype.analytics = function (i, c) { return this._uc.analytics.execute(i || {}, c || {}); };
    SearchFacade.prototype.applyRebindPolicy = function (i, c) { return this._uc.applyRebindPolicy.execute(i || {}, c || {}); };

    return SearchFacade;
});
