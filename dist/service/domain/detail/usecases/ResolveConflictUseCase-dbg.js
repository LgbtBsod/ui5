sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
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
