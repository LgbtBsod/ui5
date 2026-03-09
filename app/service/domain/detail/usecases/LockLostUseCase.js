sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/ForceReadOnlyUseCase"
], function (UseCase, Result, ForceReadOnlyUseCase) {
    "use strict";

    function LockLostUseCase() {
        UseCase.call(this, "LockLostUseCase");
        this._forceReadOnly = new ForceReadOnlyUseCase();
    }

    LockLostUseCase.prototype = Object.create(UseCase.prototype);
    LockLostUseCase.prototype.constructor = LockLostUseCase;

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

    LockLostUseCase.prototype.execute = function (mInput, mCtx) {
        var sReason = String((mInput && mInput.reason) || "killed");
        var bPreserveDirty = !!(mInput && mInput.preserveDirty);
        return this._forceReadOnly.execute({
            reason: sReason,
            messageKey: resolveMessageKey(sReason),
            preserveDirty: bPreserveDirty
        }, mCtx || {}).then(function (oResult) {
            if (!oResult || oResult.ok === false) {
                return oResult;
            }
            return Result.ok({ code: "LOCK_LOST", reason: sReason }, oResult.effects || []);
        });
    };

    return LockLostUseCase;
});
