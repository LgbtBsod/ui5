sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (Result, ForceReadOnlyUseCase, WorkflowContracts, DetailMessageKeyConstants) {
    "use strict";

    var DETAIL_MESSAGE_KEYS = DetailMessageKeyConstants;

    function LockLostUseCase() {
        var oForceReadOnly = ForceReadOnlyUseCase();
        return {
            execute: function (mInput, mCtx) {
                var sReason = String((mInput && mInput.reason) || WorkflowContracts.REASONS.KILLED);
                return oForceReadOnly.execute({
                    reason: sReason,
                    messageKey: resolveMessageKey(sReason),
                    preserveDirty: false
                }, mCtx || {}).then(function (oResult) {
                    if (!oResult || oResult.ok === false) {
                        return oResult;
                    }
                    return Result.ok({ code: WorkflowContracts.LOCK_STATES.LOCK_LOST, reason: sReason }, oResult.effects || []);
                });
            }
        };
    }

    function resolveMessageKey(sReason) {
        var sNormalized = String(sReason || WorkflowContracts.REASONS.KILLED).toUpperCase();
        if (sNormalized === WorkflowContracts.REASONS.KILLED) {
            return DETAIL_MESSAGE_KEYS.LOCK_KILLED;
        }
        if (sNormalized === WorkflowContracts.REASONS.EXPIRED || sNormalized === WorkflowContracts.REASONS.LOCK_EXPIRED) {
            return DETAIL_MESSAGE_KEYS.LOCK_EXPIRED;
        }
        return DETAIL_MESSAGE_KEYS.LOCK_LOST;
    }

    return LockLostUseCase;
});
