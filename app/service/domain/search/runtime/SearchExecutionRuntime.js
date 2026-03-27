sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/BuildSearchFilterUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ExecuteSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/RebindSearchUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/search/usecases/ApplyRebindPolicyUseCase"
], function (
    BuildSearchFilterUseCase,
    ExecuteSearchUseCase,
    RebindSearchUseCase,
    ApplyRebindPolicyUseCase
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function SearchExecutionRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            buildFilter: d.buildFilterUseCase || BuildSearchFilterUseCase(),
            execute: d.executeUseCase || ExecuteSearchUseCase(),
            rebind: d.rebindUseCase || RebindSearchUseCase(),
            applyRebindPolicy: d.applyRebindPolicyUseCase || ApplyRebindPolicyUseCase()
        };
    }

    SearchExecutionRuntime.prototype.buildFilter = function (mInput, mCtx) {
        return executeUseCase(this._uc.buildFilter, mInput, mCtx);
    };

    SearchExecutionRuntime.prototype.executeSearch = function (mInput, mCtx) {
        return executeUseCase(this._uc.execute, mInput, mCtx);
    };

    SearchExecutionRuntime.prototype.rebind = function (mInput, mCtx) {
        return executeUseCase(this._uc.rebind, mInput, mCtx);
    };

    SearchExecutionRuntime.prototype.applyRebindPolicy = function (mInput, mCtx) {
        return executeUseCase(this._uc.applyRebindPolicy, mInput, mCtx);
    };

    return SearchExecutionRuntime;
});
