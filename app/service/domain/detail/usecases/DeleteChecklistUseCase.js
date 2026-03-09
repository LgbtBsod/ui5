sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/detail/DetailAuthorizationSupport",
    "checklist/app/model/StatePaths",
    "checklist/app/util/CreateSentinel"
], function (UseCase, Result, Effects, DetailAuthorizationSupport, StatePaths, CreateSentinel) {
    "use strict";

    function resolveRootId(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        return String((mInput && mInput.rootId) || (oUiState && oUiState.get("state", "/activeObjectId")) || "").trim();
    }

    function DeleteChecklistUseCase() {
        UseCase.call(this, "DeleteChecklistUseCase");
    }

    DeleteChecklistUseCase.prototype = Object.create(UseCase.prototype);
    DeleteChecklistUseCase.prototype.constructor = DeleteChecklistUseCase;

    DeleteChecklistUseCase.prototype.execute = function (mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = resolveRootId(mInput, mCtx);

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.fail({ message: "No checklist to delete", code: "NO_CHECKLIST" }, [
                Effects.toast("nothingToDelete", "warning"),
                Effects.modelPatch("state", "/isBusy", false)
            ]));
        }

        if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Delete unavailable", code: "DELETE_UNAVAILABLE" }, [
                Effects.modelPatch("state", "/isBusy", false)
            ]));
        }

        return DetailAuthorizationSupport.fetchPermission(mCtx || {}, sRootId).then(function (oPermission) {
            if (!oPermission.canDelete) {
                return Result.fail({ message: "No permission to delete checklist", code: "NO_DELETE_PERMISSION" }, DetailAuthorizationSupport.deniedActionEffects(oPermission, "detailDeletePermissionDenied", [
                    Effects.modelPatch("state", "/isBusy", false)
                ]));
            }
            return Promise.resolve(oRepo.deleteChecklist({ rootId: sRootId })).then(function () {
                return Result.ok({ deleted: true, rootId: sRootId }, [
                    Effects.modelPatch("selected", "/", {}),
                    Effects.modelPatch("uiState", "/_detailCurrent", {}),
                    Effects.modelPatch("uiState", "/_detailSnapshot", {}),
                    Effects.modelPatch("view", "/accessState", {
                        denied: false,
                        rootId: "",
                        userId: "",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED",
                        titleKey: "",
                        messageKey: "",
                        illustrationSrc: "assets/illustrations/detail-access-denied.svg"
                    }),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch("state", "/lockOperationPending", false),
                    Effects.modelPatch("state", "/isBusy", false),
                    Effects.modelPatch("state", "/layout", "OneColumn"),
                    Effects.modelPatch("state", "/activeObjectId", null),
                    Effects.modelPatch("state", "/selectedId", null),
                    Effects.modelPatch("state", "/searchForceRefreshOnReturn", true),
                    Effects.toast("checklistDeleted", "success"),
                    Effects.navigate("search", {}, true)
                ]);
            });
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", "/isBusy", false)
            ]);
        });
    };

    return DeleteChecklistUseCase;
});
