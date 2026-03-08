sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/shared/StatePaths"
], function (UseCase, Result, Effects, StatePaths) {
    "use strict";

    function ResolveConflictUseCase() {
        UseCase.call(this, "ResolveConflictUseCase");
    }

    ResolveConflictUseCase.prototype = Object.create(UseCase.prototype);
    ResolveConflictUseCase.prototype.constructor = ResolveConflictUseCase;

    ResolveConflictUseCase.prototype.execute = function (mInput) {
        var sIntent = String((mInput && mInput.intent) || "open");
        if (sIntent === "dialogClosed") {
            return Promise.resolve(Result.ok({}, [Effects.modelPatch("state", StatePaths.UI_FEEDBACK_CONFLICT_DIALOG, null)]));
        }
        return Promise.resolve(Result.ok({}, [
            Effects.dialog("conflict", "open", { intent: sIntent }),
            Effects.modelPatch("state", StatePaths.UI_FEEDBACK_CONFLICT_DIALOG, { open: true, intent: sIntent })
        ]));
    };

    return ResolveConflictUseCase;
});
