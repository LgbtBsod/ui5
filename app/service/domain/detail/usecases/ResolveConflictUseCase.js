sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Result, Effects, StatePaths, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function ResolveConflictUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput) {
        var sIntent = String((mInput && mInput.intent) || "open");
        if (sIntent === "dialogClosed") {
            return Promise.resolve(Result.ok({}, [Effects.modelPatch(STATE_MODEL, StatePaths.UI_FEEDBACK_CONFLICT_DIALOG, null)]));
        }
        return Promise.resolve(Result.ok({}, [
            Effects.dialog("conflict", "open", { intent: sIntent }),
            Effects.modelPatch(STATE_MODEL, StatePaths.UI_FEEDBACK_CONFLICT_DIALOG, { open: true, intent: sIntent })
        ]));
    }

    return ResolveConflictUseCase;
});
