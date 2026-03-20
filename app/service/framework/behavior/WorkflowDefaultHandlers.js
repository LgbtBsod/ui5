sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/WorkflowBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (LockAdapter, DialogOrchestrator, StatePaths, RootIdRuntime, ModelStateRuntime, WorkflowBehaviorHelpers, BehaviorRegistry, CreateSentinel, WorkflowContracts) {
    "use strict";

    var WORKFLOW_SCOPE = "workflow";
    var RESULT_SAVE = "SAVE";
    var RESULT_DISCARD = "DISCARD";
    var RESULT_CANCEL = "CANCEL";
    var RESULT_NO_CHANGES = "NO_CHANGES";
    var RESULT_SAVE_FAILED = "SAVE_FAILED";
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

    function releaseWithTrySave(mContext) {
        var oController = mContext && mContext.controller;
        var sRootId = RootIdRuntime.resolveFromStateModel(ModelStateRuntime.model(oController, "state"));
        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(null);
        }
        return LockAdapter.release({
            rootId: sRootId,
            sessionGuid: ModelStateRuntime.read(oController, "state", StatePaths.SESSION_ID, ""),
            payload: (mContext && mContext.payload) || {}
        }).catch(function () {
            return null;
        });
    }

    function resolveUnsavedAction(sAction, mContext) {
        var oController = mContext && mContext.controller;
        var fnOnSave = mContext && mContext.onSave;
        var fnOnCancel = mContext && mContext.onCancel;
        if (sAction === DialogOrchestrator.actions.YES) {
            return Promise.resolve(fnOnSave && fnOnSave()).then(function (vSaveResult) {
                return (vSaveResult === false || (vSaveResult && vSaveResult.ok === false)) ? RESULT_SAVE_FAILED : RESULT_SAVE;
            }).catch(function () {
                return RESULT_SAVE_FAILED;
            });
        }
        if (sAction === DialogOrchestrator.actions.NO) {
            return releaseWithTrySave(mContext).then(function () {
                WorkflowBehaviorHelpers.resetDetailWorkflowState(oController);
                return RESULT_DISCARD;
            });
        }
        return Promise.resolve(typeof fnOnCancel === "function" ? fnOnCancel() : null).then(function () {
            return RESULT_CANCEL;
        });
    }

    function confirmUnsavedAndHandle(mContext) {
        var oController = mContext && mContext.controller;
        if (!ModelStateRuntime.read(oController, "state", "/isDirty", false)) {
            return Promise.resolve(RESULT_NO_CHANGES);
        }
        return DialogOrchestrator.promptConfirm(
            WorkflowBehaviorHelpers.resolveText(oController, "unsavedChangesPrompt", [], "unsavedChangesPrompt"),
            [DialogOrchestrator.actions.YES, DialogOrchestrator.actions.NO, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.YES
        ).then(function (sAction) {
            return resolveUnsavedAction(sAction, mContext);
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
                WorkflowBehaviorHelpers.resolveText(oController, "reloadButton", [], "reloadButton"),
                WorkflowBehaviorHelpers.resolveText(oController, "overwriteButton", [], "overwriteButton"),
                DialogOrchestrator.actions.CANCEL
            ];
            return WorkflowBehaviorHelpers.promptWarning(
                oController,
                "conflictDialogText",
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
            "genericOperationFailed",
            [((oError && oError.message) || "Unknown error")],
            "genericOperationFailed"
        );
        return Promise.resolve(null);
    }

    function confirmStealOwnLock(mContext) {
        var oController = mContext && mContext.controller;
        var sYes = WorkflowBehaviorHelpers.resolveText(oController, "yesButton", [], "yesButton");
        var sNo = WorkflowBehaviorHelpers.resolveText(oController, "noButton", [], "noButton");
        return WorkflowBehaviorHelpers.promptWarning(
            oController,
            "lockStealOwnSessionPrompt",
            [sYes, sNo],
            sYes,
            [],
            "lockStealOwnSessionPrompt"
        ).then(function (sAction) {
            return sAction === sYes;
        });
    }

    function showLockKilledNotice(mContext) {
        var oController = mContext && mContext.controller;
        return WorkflowBehaviorHelpers.promptWarning(
            oController,
            "lockKilledMessage",
            [WorkflowBehaviorHelpers.resolveText(oController, "okButton", [], "okButton")],
            undefined,
            [],
            "lockKilledMessage"
        );
    }

    var mHandlers = {
        extractBackendDetail: extractBackendDetail,
        releaseWithTrySave: releaseWithTrySave,
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
