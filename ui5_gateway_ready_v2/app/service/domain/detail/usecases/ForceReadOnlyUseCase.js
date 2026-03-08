sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/shared/StatePaths"
], function (UseCase, Result, Effects, StatePaths) {
    "use strict";

    function ForceReadOnlyUseCase() {
        UseCase.call(this, "ForceReadOnlyUseCase");
    }

    ForceReadOnlyUseCase.prototype = Object.create(UseCase.prototype);
    ForceReadOnlyUseCase.prototype.constructor = ForceReadOnlyUseCase;

    ForceReadOnlyUseCase.prototype.execute = function (mInput) {
        var sReason = String((mInput && mInput.reason) || "READ_ONLY").trim() || "READ_ONLY";
        var sMessageKey = String((mInput && mInput.messageKey) || "").trim();
        var bPreserveDirty = !!(mInput && mInput.preserveDirty);
        var aEffects = [
            Effects.modelPatch("state", StatePaths.WORKFLOW_EDIT_MODE, "READ"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_STATUS, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_LOCK_LOST_REASON, sReason),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, bPreserveDirty)
        ];

        if (sMessageKey) {
            aEffects.push(Effects.warn(sMessageKey));
        }

        return Promise.resolve(Result.ok({ forced: true, reason: sReason }, aEffects));
    };

    return ForceReadOnlyUseCase;
});
