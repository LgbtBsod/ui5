sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Effects, ActionContract, StatePaths, WorkflowContracts, DetailPersistenceRuntime, ModelContracts, DetailContracts, MessageCodeConstants, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_FLOW_CODES = MessageCodeConstants.FLOW;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
    var DETAIL_VIEW_KEYS = MessageKeyConstants.VIEW;

    function buildDiscardEffects(oUiState) {
        var oSnapshot = (oUiState && oUiState.get(MODELS.DETAIL, DETAIL_MODEL_PATHS.BASE)) || {};
        var aEffects = [
            Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT, oSnapshot),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_LOCK_LOST_REASON, ""),
            Effects.modelPatch(MODELS.STATE, StatePaths.PENDING_NAVIGATION_INTENT, null)
        ];
        return aEffects.concat(DetailPersistenceRuntime.dirtyEffects(false, {
            messageKey: DETAIL_VIEW_KEYS.PERSISTENCE_IDLE,
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
        if (sCode === DETAIL_CODES.INTEGRATION_CONFIRM_REQUIRED) {
            aEffects = (oResult.effects || []).concat([
                Effects.confirm("integrationEnterEdit", DETAIL_MESSAGE_KEYS.INTEGRATION_EDIT_CONFIRM, {
                    confirmAction: ActionContract.ACTIONS.DETAIL_CONFIRM_ENTER_EDIT,
                    cancelAction: ActionContract.ACTIONS.DETAIL_CANCEL_ENTER_EDIT,
                    payload: {
                        rootId: (mInput && mInput.rootId) || "",
                        state: true,
                        confirmedIntegration: true
                    }
                })
            ]);
            return Object.assign({}, oResult, { effects: aEffects });
        }
        if (sCode !== DETAIL_FLOW_CODES.LOCKED_OWN_SESSION && sCode !== DETAIL_CODES.EXPIRED) {
            return oResult;
        }
        sTextKey = sCode === DETAIL_CODES.EXPIRED
            ? DETAIL_MESSAGE_KEYS.LOCK_EXPIRED_TAKEOVER_PROMPT
            : DETAIL_MESSAGE_KEYS.LOCK_STEAL_OWN_SESSION_PROMPT;
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
                Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
                Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false)
            ]
        });
    }

    return {
        buildCancelEnterEditResult: buildCancelEnterEditResult,
        buildDiscardEffects: buildDiscardEffects,
        decorateEnterEditResult: decorateEnterEditResult
    };
});
