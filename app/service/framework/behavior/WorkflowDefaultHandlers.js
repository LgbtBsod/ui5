sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (LockAdapter, DialogOrchestrator, StatePaths, FeedbackCoordinator, RootIdRuntime, ModelStateRuntime, BehaviorRegistry, WorkflowContracts) {
    "use strict";

    var WORKFLOW_SCOPE = "workflow";
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
        return LockAdapter.create().release({
            rootId: RootIdRuntime.resolveFromStateModel(ModelStateRuntime.model(oController, "state")),
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
                return (vSaveResult === false || (vSaveResult && vSaveResult.ok === false)) ? "SAVE_FAILED" : "SAVE";
            }).catch(function () {
                return "SAVE_FAILED";
            });
        }
        if (sAction === DialogOrchestrator.actions.NO) {
            return releaseWithTrySave(mContext).then(function () {
                ModelStateRuntime.resetDetailWorkflowState(oController, {
                    [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE]: ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE) || WorkflowContracts.AUTOSAVE_STATES.IDLE,
                    [StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT]: ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                    [StatePaths.WORKFLOW_AUTOSAVE_ENABLED]: ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false) === true,
                    "/activeObjectId": ModelStateRuntime.read(oController, "state", "/activeObjectId", "") || ""
                });
                return "DISCARD";
            });
        }
        return Promise.resolve(typeof fnOnCancel === "function" ? fnOnCancel() : null).then(function () {
            return "CANCEL";
        });
    }

    function confirmUnsavedAndHandle(mContext) {
        var oController = mContext && mContext.controller;
        var sText;
        if (!ModelStateRuntime.read(oController, "state", "/isDirty", false)) {
            return Promise.resolve("NO_CHANGES");
        }
        sText = FeedbackCoordinator.resolveText(oController, "unsavedChangesPrompt", [], "unsavedChangesPrompt");
        return DialogOrchestrator.promptConfirm(
            sText,
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

        if (iStatus === 409) {
            aActions = [
                FeedbackCoordinator.resolveText(oController, "reloadButton", [], "reloadButton"),
                FeedbackCoordinator.resolveText(oController, "overwriteButton", [], "overwriteButton"),
                DialogOrchestrator.actions.CANCEL
            ];
            return DialogOrchestrator.promptWarning(
                FeedbackCoordinator.resolveText(oController, "conflictDialogText", [], "conflictDialogText"),
                aActions
            ).then(function (sAction) {
                return mHandlers && mHandlers.onConflictChoice ? mHandlers.onConflictChoice(sAction) : sAction;
            });
        }
        if (iStatus === 410 && mHandlers && mHandlers.onLockExpired) {
            return mHandlers.onLockExpired();
        }
        DialogOrchestrator.promptError(
            FeedbackCoordinator.resolveText(
                oController,
                "genericOperationFailed",
                [((oError && oError.message) || "Unknown error")],
                "genericOperationFailed"
            )
        );
        return Promise.resolve(null);
    }

    function confirmStealOwnLock(mContext) {
        var oController = mContext && mContext.controller;
        var sYes = FeedbackCoordinator.resolveText(oController, "yesButton", [], "yesButton");
        var sNo = FeedbackCoordinator.resolveText(oController, "noButton", [], "noButton");
        return DialogOrchestrator.promptWarning(
            FeedbackCoordinator.resolveText(oController, "lockStealOwnSessionPrompt", [], "lockStealOwnSessionPrompt"),
            [sYes, sNo],
            sYes
        ).then(function (sAction) {
            return sAction === sYes;
        });
    }

    function showLockKilledNotice(mContext) {
        var oController = mContext && mContext.controller;
        DialogOrchestrator.promptWarning(
            FeedbackCoordinator.resolveText(oController, "lockKilledMessage", [], "lockKilledMessage"),
            [FeedbackCoordinator.resolveText(oController, "okButton", [], "okButton")]
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
