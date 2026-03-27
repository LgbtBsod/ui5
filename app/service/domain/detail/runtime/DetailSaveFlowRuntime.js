sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/SaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValidateChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/DeleteChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ResolveConflictUseCase"
], function (
    SaveDetailUseCase,
    ValidateChecklistUseCase,
    AutosaveDetailUseCase,
    DeleteChecklistUseCase,
    ResolveConflictUseCase
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function DetailSaveFlowRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            save: d.saveUseCase || SaveDetailUseCase(),
            validate: d.validateUseCase || ValidateChecklistUseCase(),
            autosave: d.autosaveUseCase || AutosaveDetailUseCase(),
            deleteChecklist: d.deleteChecklistUseCase || DeleteChecklistUseCase(),
            resolveConflict: d.resolveConflictUseCase || ResolveConflictUseCase()
        };
    }

    DetailSaveFlowRuntime.prototype.save = function (mInput, mCtx) {
        return executeUseCase(this._uc.save, mInput, mCtx);
    };

    DetailSaveFlowRuntime.prototype.validate = function (mInput, mCtx) {
        return executeUseCase(this._uc.validate, mInput, mCtx);
    };

    DetailSaveFlowRuntime.prototype.autosave = function (mInput, mCtx) {
        return executeUseCase(this._uc.autosave, mInput, mCtx);
    };

    DetailSaveFlowRuntime.prototype.deleteChecklist = function (mInput, mCtx) {
        return executeUseCase(this._uc.deleteChecklist, mInput, mCtx);
    };

    DetailSaveFlowRuntime.prototype.resolveConflict = function (mInput, mCtx) {
        return executeUseCase(this._uc.resolveConflict, mInput, mCtx);
    };

    return DetailSaveFlowRuntime;
});
