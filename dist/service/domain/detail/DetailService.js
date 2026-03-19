sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
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
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (
    CtxFactory,
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
    StatePaths,
    WorkflowContracts,
    DetailPersistenceRuntime
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function buildDiscardEffects(oUiState) {
        var oSnapshot = (oUiState && oUiState.get("snapshot", "/")) || {};
        var aEffects = [
            Effects.modelPatch("selected", "/", oSnapshot),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, ""),
            Effects.modelPatch("state", StatePaths.PENDING_NAVIGATION_INTENT, null)
        ];
        return aEffects.concat(DetailPersistenceRuntime.dirtyEffects(false, {
            messageKey: "persistenceIdle",
            lastSaveError: null,
            taxonomy: "",
            currentWriteRequestId: "",
            isManualSaveInFlight: false,
            isAutoSaveInFlight: false
        }));
    }

    function DetailService(oController, mDeps) {
        var d = mDeps || {};
        this._controller = oController || null;
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

    DetailService.prototype._buildCtx = function () {
        return CtxFactory.buildCtx(this._controller, {});
    };

    DetailService.prototype.openDetail = function (mInput, mCtx) {
        return executeUseCase(this._uc.open, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.enterEdit = function (mInput, mCtx) {
        return executeUseCase(this._uc.enterEdit, mInput, mCtx || this._buildCtx()).then(function (oResult) {
            var sCode = (oResult && oResult.error && oResult.error.code) || (oResult && oResult.data && oResult.data.code) || "";
            var sTextKey;
            var aEffects;
            if (sCode !== "LOCKED_OWN_SESSION" && sCode !== "EXPIRED") {
                return oResult;
            }
            sTextKey = sCode === "EXPIRED" ? "lockExpiredTakeoverPrompt" : "lockStealOwnSessionPrompt";
            aEffects = (oResult.effects || []).concat([
                Effects.confirm("takeoverOwnLock", sTextKey, {
                    confirmAction: ActionContract.ACTIONS.DETAIL_TAKEOVER_LOCK,
                    cancelAction: ActionContract.ACTIONS.DETAIL_CANCEL_ENTER_EDIT,
                    payload: { rootId: (mInput && mInput.rootId) || "" }
                })
            ]);
            return Object.assign({}, oResult, { effects: aEffects });
        });
    };

    DetailService.prototype.saveDetail = function (mInput, mCtx) {
        return executeUseCase(this._uc.save, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.autosaveDetail = function (mInput, mCtx) {
        return executeUseCase(this._uc.autosave, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.changeStatus = function (mInput, mCtx) {
        return executeUseCase(this._uc.changeStatus, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.loadAttachments = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentLoad, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.rowAction = function (mInput, mCtx) {
        return executeUseCase(this._uc.rowOps, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.handleLockLost = function (mInput, mCtx) {
        return executeUseCase(this._uc.lockLost, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.confirmTakeover = function (mInput, mCtx) {
        return executeUseCase(this._uc.takeoverLock, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.cancelEnterEdit = function () {
        return Promise.resolve({
            ok: true,
            effects: [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false)
            ]
        });
    };

    DetailService.prototype.discardChanges = function (_mInput, mCtx) {
        var oCtx = mCtx || this._buildCtx();
        return Promise.resolve({
            ok: true,
            effects: buildDiscardEffects(oCtx && oCtx.uiState)
        });
    };

    DetailService.prototype.forceReadOnly = function (mInput, mCtx) {
        return executeUseCase(this._uc.forceReadOnly, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.close = function (mInput, mCtx) {
        return executeUseCase(this._uc.close, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.validate = function (mInput, mCtx) {
        return executeUseCase(this._uc.validate, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.deleteChecklist = function (mInput, mCtx) {
        return executeUseCase(this._uc.deleteChecklist, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.resolveConflict = function (mInput, mCtx) {
        return executeUseCase(this._uc.resolveConflict, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.attachmentUpload = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentUpload, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.attachmentDelete = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentDelete, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.valueHelpLocation = function (mInput, mCtx) {
        return executeUseCase(this._uc.valueHelpLocation, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.personSuggest = function (mInput, mCtx) {
        return executeUseCase(this._uc.personSuggest, mInput, mCtx || this._buildCtx());
    };

    DetailService.prototype.open = DetailService.prototype.openDetail;
    DetailService.prototype.save = DetailService.prototype.saveDetail;
    DetailService.prototype.autosave = DetailService.prototype.autosaveDetail;
    DetailService.prototype.attachmentLoad = DetailService.prototype.loadAttachments;
    DetailService.prototype.rowOps = DetailService.prototype.rowAction;
    DetailService.prototype.onLockLost = DetailService.prototype.handleLockLost;

    return DetailService;
});
