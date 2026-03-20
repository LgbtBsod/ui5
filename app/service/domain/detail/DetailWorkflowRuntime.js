sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime"
], function (Effects, ActionContract, StatePaths, WorkflowContracts, DetailPersistenceRuntime) {
    "use strict";

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

    function decorateEnterEditResult(oResult, mInput) {
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
    }

    function buildCancelEnterEditResult() {
        return Promise.resolve({
            ok: true,
            effects: [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false)
            ]
        });
    }

    return {
        buildCancelEnterEditResult: buildCancelEnterEditResult,
        buildDiscardEffects: buildDiscardEffects,
        decorateEnterEditResult: decorateEnterEditResult
    };
});
