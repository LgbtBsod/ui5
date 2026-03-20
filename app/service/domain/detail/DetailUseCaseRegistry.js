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
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/lock/usecases/TakeoverLockUseCase"
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
    TakeoverLockUseCase
) {
    "use strict";

    function create(mDeps) {
        var d = mDeps || {};
        return {
            open: d.openUseCase || new OpenDetailUseCase(),
            enterEdit: d.enterEditUseCase || new EnterEditUseCase(),
            save: d.saveUseCase || new SaveDetailUseCase(),
            validate: d.validateUseCase || new ValidateChecklistUseCase(),
            autosave: d.autosaveUseCase || new AutosaveDetailUseCase(),
            close: d.closeUseCase || new CloseDetailUseCase(),
            deleteChecklist: d.deleteChecklistUseCase || new DeleteChecklistUseCase(),
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

    return {
        create: create
    };
});
