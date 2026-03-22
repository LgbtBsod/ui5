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

    return {
        create: create
    };
});
