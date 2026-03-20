sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase"
], function (Result, ForceReadOnlyUseCase) {
    "use strict";

    function LockLostUseCase() {
        var oForceReadOnly = ForceReadOnlyUseCase();
        return {
            execute: function (mInput, mCtx) {
                var sReason = String((mInput && mInput.reason) || "killed");
                return oForceReadOnly.execute({
                    reason: sReason,
                    messageKey: resolveMessageKey(sReason),
                    preserveDirty: false
                }, mCtx || {}).then(function (oResult) {
                    if (!oResult || oResult.ok === false) {
                        return oResult;
                    }
                    return Result.ok({ code: "LOCK_LOST", reason: sReason }, oResult.effects || []);
                });
            }
        };
    }

    function resolveMessageKey(sReason) {
        var sNormalized = String(sReason || "killed").toUpperCase();
        if (sNormalized === "KILLED") {
            return "lockKilledMessage";
        }
        if (sNormalized === "EXPIRED" || sNormalized === "LOCK_EXPIRED") {
            return "lockExpiredMessage";
        }
        return "lockLostMessage";
    }

    return LockLostUseCase;
});
