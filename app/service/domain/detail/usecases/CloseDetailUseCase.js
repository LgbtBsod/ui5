sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/shared/StatePaths",
    "sap_ui5/util/CreateSentinel"
], function (UseCase, Result, Effects, StatePaths, CreateSentinel) {
    "use strict";

    function CloseDetailUseCase() {
        UseCase.call(this, "CloseDetailUseCase");
    }

    CloseDetailUseCase.prototype = Object.create(UseCase.prototype);
    CloseDetailUseCase.prototype.constructor = CloseDetailUseCase;

    CloseDetailUseCase.prototype.execute = function (mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var oLockPort = mCtx && mCtx.lock;
        var sRootId = (mInput && mInput.rootId) || (oUiState && oUiState.get("state", "/activeObjectId"));
        var sSessionGuid = oUiState && oUiState.get("state", StatePaths.SESSION_ID);

        var pRelease = Promise.resolve();
        if (sRootId && !CreateSentinel.isCreateId(sRootId) && sSessionGuid && oLockPort && typeof oLockPort.release === "function") {
            pRelease = Promise.resolve(oLockPort.release({ rootId: sRootId, sessionGuid: sSessionGuid })).catch(function () { return null; });
        }

        return pRelease.then(function () {
            return Result.ok({ reason: (mInput && mInput.intent) || "close" }, [
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
                Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                Effects.modelPatch("state", "/lockOperationPending", false),
                Effects.modelPatch("state", "/layout", "OneColumn"),
                Effects.modelPatch("state", "/activeObjectId", null),
                Effects.modelPatch("state", "/selectedId", null),
                Effects.navigate("search", {}, true)
            ]);
        });
    };

    return CloseDetailUseCase;
});
