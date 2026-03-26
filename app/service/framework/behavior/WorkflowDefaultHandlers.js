sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/WorkflowBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDecisionRuntime"
], function (DialogOrchestrator, StatePaths, ModelStateRuntime, WorkflowBehaviorHelpers, BehaviorRegistry, ModelContracts, MessageKeyConstants, WorkflowDecisionRuntime) {
    "use strict";

    var WORKFLOW_SCOPE = "workflow";
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var RESULTS = WorkflowDecisionRuntime.RESULTS;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
    var VIEW_MESSAGE_KEYS = MessageKeyConstants.VIEW;
    var HTTP_CONFLICT = 409;
    var HTTP_GONE = 410;
    var bDefaultsRegistered = false;

    function statusFromError(oError) {
        var mMatch = String((oError && oError.message) || "").match(/HTTP\s+(\d+)/i);
        return mMatch ? Number(mMatch[1]) : 0;
    }

    function extractBackendDetail(mContext) {
        var sMessage = String((mContext && mContext.error && mContext.error.message) || "");
        var iJsonStart = sMessage.indexOf("{");
        if (iJsonStart < 0) {
            return null;
        }
        try {
            return JSON.parse(sMessage.slice(iJsonStart));
        } catch (_error) {
            return null;
        }
    }

    function confirmUnsavedAndHandle(mContext) {
        var oController = mContext && mContext.controller;
        if (!ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DIRTY, false)) {
            return Promise.resolve(RESULTS.NO_CHANGES);
        }
        return DialogOrchestrator.promptConfirm(
            WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.UNSAVED_CHANGES_PROMPT, [], VIEW_MESSAGE_KEYS.UNSAVED_CHANGES_PROMPT),
            [DialogOrchestrator.actions.YES, DialogOrchestrator.actions.NO, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.YES
        ).then(function (sAction) {
            return WorkflowDecisionRuntime.resolveUnsavedDecision(sAction, mContext, DialogOrchestrator);
        });
    }

    function handleBackendError(mContext) {
        var oController = mContext && mContext.controller;
        var oError = mContext && mContext.error;
        var mHandlers = mContext && mContext.handlers;
        var iStatus = statusFromError(oError);
        var aActions;

        if (iStatus === HTTP_CONFLICT) {
            aActions = [
                WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.RELOAD_BUTTON, [], VIEW_MESSAGE_KEYS.RELOAD_BUTTON),
                WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.OVERWRITE_BUTTON, [], VIEW_MESSAGE_KEYS.OVERWRITE_BUTTON),
                DialogOrchestrator.actions.CANCEL
            ];
            return WorkflowBehaviorHelpers.promptWarning(
                oController,
                VIEW_MESSAGE_KEYS.CONFLICT_DIALOG_TEXT,
                aActions
            ).then(function (sAction) {
                return mHandlers && mHandlers.onConflictChoice ? mHandlers.onConflictChoice(sAction) : sAction;
            });
        }
        if (iStatus === HTTP_GONE && mHandlers && mHandlers.onLockExpired) {
            return mHandlers.onLockExpired();
        }
        WorkflowBehaviorHelpers.promptError(
            oController,
            VIEW_MESSAGE_KEYS.GENERIC_OPERATION_FAILED,
            [((oError && oError.message) || "Unknown error")],
            VIEW_MESSAGE_KEYS.GENERIC_OPERATION_FAILED
        );
        return Promise.resolve(null);
    }

    function confirmStealOwnLock(mContext) {
        var oController = mContext && mContext.controller;
        var sYes = WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.YES_BUTTON, [], VIEW_MESSAGE_KEYS.YES_BUTTON);
        var sNo = WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.NO_BUTTON, [], VIEW_MESSAGE_KEYS.NO_BUTTON);
        return WorkflowBehaviorHelpers.promptWarning(
            oController,
            DETAIL_MESSAGE_KEYS.LOCK_STEAL_OWN_SESSION_PROMPT,
            [sYes, sNo],
            sYes,
            [],
            DETAIL_MESSAGE_KEYS.LOCK_STEAL_OWN_SESSION_PROMPT
        ).then(function (sAction) {
            return sAction === sYes;
        });
    }

    function showLockKilledNotice(mContext) {
        var oController = mContext && mContext.controller;
        return WorkflowBehaviorHelpers.promptWarning(
            oController,
            DETAIL_MESSAGE_KEYS.LOCK_KILLED,
            [WorkflowBehaviorHelpers.resolveText(oController, VIEW_MESSAGE_KEYS.OK_BUTTON, [], VIEW_MESSAGE_KEYS.OK_BUTTON)],
            undefined,
            [],
            DETAIL_MESSAGE_KEYS.LOCK_KILLED
        );
    }

    var mHandlers = {
        extractBackendDetail: extractBackendDetail,
        releaseActiveLock: WorkflowDecisionRuntime.releaseActiveLock,
        releaseWithTrySave: WorkflowDecisionRuntime.releaseWithTrySave,
        confirmUnsavedAndHandle: confirmUnsavedAndHandle,
        handleBackendError: handleBackendError,
        confirmStealOwnLock: confirmStealOwnLock,
        showLockKilledNotice: showLockKilledNotice
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(WORKFLOW_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
