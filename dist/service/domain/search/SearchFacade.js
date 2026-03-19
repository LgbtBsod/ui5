sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/InitializeSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/BuildSearchFilterUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExecuteSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/RebindSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectRowUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/SelectionChangedUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExportSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/AnalyticsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ApplyRebindPolicyUseCase"
], function (
    InitializeSearchUseCase,
    BuildSearchFilterUseCase,
    ExecuteSearchUseCase,
    RebindSearchUseCase,
    SelectRowUseCase,
    SelectionChangedUseCase,
    ExportSearchUseCase,
    AnalyticsUseCase,
    ApplyRebindPolicyUseCase
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function SearchFacade(mDeps) {
        var d = mDeps || {};
        this._uc = {
            bootstrap: d.bootstrapUseCase || new InitializeSearchUseCase(),
            buildFilter: d.buildFilterUseCase || new BuildSearchFilterUseCase(),
            execute: d.executeUseCase || new ExecuteSearchUseCase(),
            rebind: d.rebindUseCase || new RebindSearchUseCase(),
            selectRow: d.selectRowUseCase || new SelectRowUseCase(),
            selectionChanged: d.selectionChangedUseCase || new SelectionChangedUseCase(),
            exportFlow: d.exportUseCase || new ExportSearchUseCase(),
            analytics: d.analyticsUseCase || new AnalyticsUseCase(),
            applyRebindPolicy: d.applyRebindPolicyUseCase || new ApplyRebindPolicyUseCase()
        };
    }

    SearchFacade.prototype.bootstrap = function (i, c) { return executeUseCase(this._uc.bootstrap, i, c); };
    SearchFacade.prototype.buildFilter = function (i, c) { return executeUseCase(this._uc.buildFilter, i, c); };
    SearchFacade.prototype.executeSearch = function (i, c) { return executeUseCase(this._uc.execute, i, c); };
    SearchFacade.prototype.rebind = function (i, c) { return executeUseCase(this._uc.rebind, i, c); };
    SearchFacade.prototype.selectRow = function (i, c) { return executeUseCase(this._uc.selectRow, i, c); };
    SearchFacade.prototype.selectionChanged = function (i, c) { return executeUseCase(this._uc.selectionChanged, i, c); };
    SearchFacade.prototype.exportFlow = function (i, c) { return executeUseCase(this._uc.exportFlow, i, c); };
    SearchFacade.prototype.analytics = function (i, c) { return executeUseCase(this._uc.analytics, i, c); };
    SearchFacade.prototype.applyRebindPolicy = function (i, c) { return executeUseCase(this._uc.applyRebindPolicy, i, c); };

    return SearchFacade;
});
