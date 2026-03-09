sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/OpenDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/EnterEditUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/SaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ValidateChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AutosaveDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/CloseDetailUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/DeleteChecklistUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ChangeStatusUseCase",
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
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (
    OpenDetailUseCase,
    EnterEditUseCase,
    SaveDetailUseCase,
    ValidateChecklistUseCase,
    AutosaveDetailUseCase,
    CloseDetailUseCase,
    DeleteChecklistUseCase,
    ChangeStatusUseCase,
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
    Effects,
    ActionContract,
    StatePaths
) {
    "use strict";

    function DetailFacade(mDeps) {
        var d = mDeps || {};
        this._uc = {
            open: d.openUseCase || new OpenDetailUseCase(),
            enterEdit: d.enterEditUseCase || new EnterEditUseCase(),
            save: d.saveUseCase || new SaveDetailUseCase(),
            validate: d.validateUseCase || new ValidateChecklistUseCase(),
            autosave: d.autosaveUseCase || new AutosaveDetailUseCase(),
            close: d.closeUseCase || new CloseDetailUseCase(),
            deleteChecklist: d.deleteChecklistUseCase || new DeleteChecklistUseCase(),
            changeStatus: d.changeStatusUseCase || new ChangeStatusUseCase(),
            resolveConflict: d.resolveConflictUseCase || new ResolveConflictUseCase(),
            attachmentLoad: d.attachmentLoadUseCase || new LoadAttachmentsUseCase(),
            attachmentUpload: d.attachmentUploadUseCase || new AttachmentUploadUseCase(),
            attachmentDelete: d.attachmentDeleteUseCase || new AttachmentDeleteUseCase(),
            rowOps: d.rowOpsUseCase || new RowOpsUseCase(),
            valueHelpLocation: d.valueHelpLocationUseCase || new ValueHelpLocationUseCase(),
            personSuggest: d.personSuggestUseCase || new PersonSuggestUseCase(),
            lockLost: d.lockLostUseCase || new LockLostUseCase(),
            forceReadOnly: d.forceReadOnlyUseCase || new ForceReadOnlyUseCase(),
            takeoverLock: d.takeoverLockUseCase || new TakeoverLockUseCase()
        };
    }

    DetailFacade.prototype.open = function (i, c) { return this._uc.open.execute(i || {}, c || {}); };

    DetailFacade.prototype.enterEdit = function (i, c) {
        return this._uc.enterEdit.execute(i || {}, c || {}).then(function (oResult) {
            var sCode = (oResult && oResult.error && oResult.error.code) || (oResult && oResult.data && oResult.data.code) || "";
            var sTextKey;
            if (sCode !== "LOCKED_OWN_SESSION" && sCode !== "EXPIRED") {
                return oResult;
            }
            sTextKey = sCode === "EXPIRED" ? "lockExpiredTakeoverPrompt" : "lockStealOwnSessionPrompt";
            var aEffects = (oResult.effects || []).concat([
                Effects.confirm("takeoverOwnLock", sTextKey, {
                    confirmAction: ActionContract.ACTIONS.DETAIL_TAKEOVER_LOCK,
                    cancelAction: ActionContract.ACTIONS.DETAIL_CANCEL_ENTER_EDIT,
                    payload: { rootId: (i && i.rootId) || "" }
                })
            ]);
            return Object.assign({}, oResult, { effects: aEffects });
        });
    };

    DetailFacade.prototype.confirmTakeover = function (i, c) {
        return this._uc.takeoverLock.execute(i || {}, c || {});
    };

    DetailFacade.prototype.cancelEnterEdit = function (_i, _c) {
        return Promise.resolve({ ok: true, effects: [
            Effects.modelPatch("state", StatePaths.WORKFLOW_EDIT_MODE, "READ"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_STATUS, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false)
        ]});
    };

    DetailFacade.prototype.discardChanges = function (_i, c) {
        var oUiState = c && c.uiState;
        var oSnapshot = (oUiState && oUiState.get("uiState", "/_detailSnapshot")) || {};
        return Promise.resolve({ ok: true, effects: [
            Effects.modelPatch("selected", "/", oSnapshot),
            Effects.modelPatch("uiState", "/_detailCurrent", oSnapshot),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE")
        ]});
    };

    DetailFacade.prototype.onLockLost = function (i, c) {
        return this._uc.lockLost.execute(i || {}, c || {});
    };

    DetailFacade.prototype.forceReadOnly = function (i, c) {
        return this._uc.forceReadOnly.execute(i || {}, c || {});
    };

    DetailFacade.prototype.closeDetail = function (i, c) {
        return this._uc.close.execute(i || {}, c || {});
    };

    DetailFacade.prototype.save = function (i, c) { return this._uc.save.execute(i || {}, c || {}); };
    DetailFacade.prototype.validate = function (i, c) { return this._uc.validate.execute(i || {}, c || {}); };
    DetailFacade.prototype.autosave = function (i, c) { return this._uc.autosave.execute(i || {}, c || {}); };
    DetailFacade.prototype.close = function (i, c) { return this.closeDetail(i, c); };
    DetailFacade.prototype.deleteChecklist = function (i, c) { return this._uc.deleteChecklist.execute(i || {}, c || {}); };
    DetailFacade.prototype.changeStatus = function (i, c) { return this._uc.changeStatus.execute(i || {}, c || {}); };
    DetailFacade.prototype.resolveConflict = function (i, c) { return this._uc.resolveConflict.execute(i || {}, c || {}); };
    DetailFacade.prototype.attachmentLoad = function (i, c) { return this._uc.attachmentLoad.execute(i || {}, c || {}); };
    DetailFacade.prototype.attachmentUpload = function (i, c) { return this._uc.attachmentUpload.execute(i || {}, c || {}); };
    DetailFacade.prototype.attachmentDelete = function (i, c) { return this._uc.attachmentDelete.execute(i || {}, c || {}); };
    DetailFacade.prototype.rowOps = function (i, c) { return this._uc.rowOps.execute(i || {}, c || {}); };
    DetailFacade.prototype.valueHelpLocation = function (i, c) { return this._uc.valueHelpLocation.execute(i || {}, c || {}); };
    DetailFacade.prototype.personSuggest = function (i, c) { return this._uc.personSuggest.execute(i || {}, c || {}); };

    return DetailFacade;
});
