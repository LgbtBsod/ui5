sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/OpenDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/EnterEditUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/SaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValidateChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/CloseDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/DeleteChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ResolveConflictUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/LoadAttachmentsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AttachmentUploadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AttachmentDeleteUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/RowOpsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValueHelpLocationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/PersonSuggestUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/LockLostUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/lock/usecases/TakeoverLockUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailWorkflowRuntime"
], function (
    OpenDetailUseCase,
    EnterEditUseCase,
    SaveDetailUseCase,
    ValidateChecklistUseCase,
    AutosaveDetailUseCase,
    CloseDetailUseCase,
    DeleteChecklistUseCase,
    ResolveConflictUseCase,
    LoadAttachmentsUseCase,
    AttachmentUploadUseCase,
    AttachmentDeleteUseCase,
    RowOpsUseCase,
    ValueHelpLocationUseCase,
    PersonSuggestUseCase,
    LockLostUseCase,
    ForceReadOnlyUseCase,
    TakeoverLockUseCase,
    DetailWorkflowRuntime
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function decorateEnterEdit(oPromise, mInput) {
        return Promise.resolve(oPromise).then(function (oResult) {
            return DetailWorkflowRuntime.decorateEnterEditResult(oResult, mInput);
        });
    }

    function buildUseCases(mDeps) {
        var d = mDeps || {};
        return {
            open: d.openUseCase || OpenDetailUseCase(),
            enterEdit: d.enterEditUseCase || EnterEditUseCase(),
            save: d.saveUseCase || SaveDetailUseCase(),
            validate: d.validateUseCase || ValidateChecklistUseCase(),
            autosave: d.autosaveUseCase || AutosaveDetailUseCase(),
            close: d.closeUseCase || CloseDetailUseCase(),
            deleteChecklist: d.deleteChecklistUseCase || DeleteChecklistUseCase(),
            resolveConflict: d.resolveConflictUseCase || ResolveConflictUseCase(),
            attachmentLoad: d.attachmentLoadUseCase || LoadAttachmentsUseCase(),
            attachmentUpload: d.attachmentUploadUseCase || AttachmentUploadUseCase(),
            attachmentDelete: d.attachmentDeleteUseCase || AttachmentDeleteUseCase(),
            rowOps: d.rowOpsUseCase || RowOpsUseCase(),
            valueHelpLocation: d.valueHelpLocationUseCase || ValueHelpLocationUseCase(),
            personSuggest: d.personSuggestUseCase || PersonSuggestUseCase(),
            lockLost: d.lockLostUseCase || LockLostUseCase(),
            forceReadOnly: d.forceReadOnlyUseCase || ForceReadOnlyUseCase(),
            takeoverLock: d.takeoverLockUseCase || TakeoverLockUseCase()
        };
    }

    function DetailFacade(mDeps) {
        this._uc = buildUseCases(mDeps);
    }

    DetailFacade.prototype.open = function (i, c) { return executeUseCase(this._uc.open, i, c); };

    DetailFacade.prototype.enterEdit = function (i, c) {
        return decorateEnterEdit(executeUseCase(this._uc.enterEdit, i, c), i);
    };

    DetailFacade.prototype.confirmTakeover = function (i, c) {
        return executeUseCase(this._uc.takeoverLock, i, c);
    };

    DetailFacade.prototype.cancelEnterEdit = function (_i, _c) {
        return DetailWorkflowRuntime.buildCancelEnterEditResult();
    };

    DetailFacade.prototype.discardChanges = function (_i, c) {
        return Promise.resolve({
            ok: true,
            effects: DetailWorkflowRuntime.buildDiscardEffects(c && c.uiState)
        });
    };

    DetailFacade.prototype.onLockLost = function (i, c) {
        return executeUseCase(this._uc.lockLost, i, c);
    };

    DetailFacade.prototype.forceReadOnly = function (i, c) {
        return executeUseCase(this._uc.forceReadOnly, i, c);
    };

    DetailFacade.prototype.close = function (i, c) { return executeUseCase(this._uc.close, i, c); };
    DetailFacade.prototype.closeDetail = function (i, c) { return this.close(i, c); };
    DetailFacade.prototype.save = function (i, c) { return executeUseCase(this._uc.save, i, c); };
    DetailFacade.prototype.validate = function (i, c) { return executeUseCase(this._uc.validate, i, c); };
    DetailFacade.prototype.autosave = function (i, c) { return executeUseCase(this._uc.autosave, i, c); };
    DetailFacade.prototype.deleteChecklist = function (i, c) { return executeUseCase(this._uc.deleteChecklist, i, c); };
    DetailFacade.prototype.resolveConflict = function (i, c) { return executeUseCase(this._uc.resolveConflict, i, c); };
    DetailFacade.prototype.attachmentLoad = function (i, c) { return executeUseCase(this._uc.attachmentLoad, i, c); };
    DetailFacade.prototype.attachmentUpload = function (i, c) { return executeUseCase(this._uc.attachmentUpload, i, c); };
    DetailFacade.prototype.attachmentDelete = function (i, c) { return executeUseCase(this._uc.attachmentDelete, i, c); };
    DetailFacade.prototype.rowOps = function (i, c) { return executeUseCase(this._uc.rowOps, i, c); };
    DetailFacade.prototype.valueHelpLocation = function (i, c) { return executeUseCase(this._uc.valueHelpLocation, i, c); };
    DetailFacade.prototype.personSuggest = function (i, c) { return executeUseCase(this._uc.personSuggest, i, c); };

    return DetailFacade;
});
