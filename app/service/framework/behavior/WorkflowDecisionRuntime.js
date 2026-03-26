sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/WorkflowBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (
    LockAdapter,
    StatePaths,
    ModelStateRuntime,
    WorkflowBehaviorHelpers,
    CreateSentinel,
    ModelContracts,
    MessageCodeConstants,
    ModelPathContracts
) {
    "use strict";

    var RESULTS = Object.freeze({
        SAVE: "SAVE",
        DISCARD: "DISCARD",
        CANCEL: "CANCEL",
        NO_CHANGES: MessageCodeConstants.FLOW.NO_CHANGES,
        SAVE_FAILED: "SAVE_FAILED"
    });
    var STATE_MODEL = ModelContracts.MODELS.STATE;

    /* Этот блок освобождает активную блокировку для текущего detail-объекта.
     * Результат: при выходе или отказе от изменений backend-lock корректно снимается. */
    function releaseActiveLock(mContext) {
        var oController = mContext && mContext.controller;
        var oStateModel = ModelStateRuntime.model(oController, STATE_MODEL);
        var sRootId = String(
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "")
            || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, "")
            || ""
        ).trim();

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(null);
        }

        return LockAdapter.release({
            rootId: sRootId,
            sessionGuid: ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.SESSION_ID, ""),
            payload: (mContext && mContext.payload) || {}
        }).catch(function () {
            return null;
        });
    }

    /* Этот блок сохраняет совместимость со старым именем releaseWithTrySave.
     * Результат: старые call sites не падают, но фактическая семантика остается простой release. */
    function releaseWithTrySave(mContext) {
        return releaseActiveLock(mContext);
    }

    /* Этот блок принимает решение по диалогу несохраненных изменений.
     * Результат: сценарий save, discard или cancel завершается единым каноническим результатом. */
    function resolveUnsavedDecision(sAction, mContext, DialogOrchestrator) {
        var oController = mContext && mContext.controller;
        var fnOnSave = mContext && mContext.onSave;
        var fnOnCancel = mContext && mContext.onCancel;

        if (sAction === DialogOrchestrator.actions.YES) {
            return Promise.resolve(fnOnSave && fnOnSave()).then(function (vSaveResult) {
                return (vSaveResult === false || (vSaveResult && vSaveResult.ok === false)) ? RESULTS.SAVE_FAILED : RESULTS.SAVE;
            }).catch(function () {
                return RESULTS.SAVE_FAILED;
            });
        }

        if (sAction === DialogOrchestrator.actions.NO) {
            return releaseActiveLock(mContext).then(function () {
                WorkflowBehaviorHelpers.resetDetailWorkflowState(oController);
                return RESULTS.DISCARD;
            });
        }

        return Promise.resolve(typeof fnOnCancel === "function" ? fnOnCancel() : null).then(function () {
            return RESULTS.CANCEL;
        });
    }

    return {
        RESULTS: RESULTS,
        releaseActiveLock: releaseActiveLock,
        releaseWithTrySave: releaseWithTrySave,
        resolveUnsavedDecision: resolveUnsavedDecision
    };
});
