sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime"
], function (Result, Effects, DetailAuthorizationRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, ViewPathContracts, ModelPathContracts, NavigationContracts, WorkflowContracts, WorkflowRuntimeConstants, SearchReturnRediscoveryRuntime) {
    "use strict";

    function DeleteChecklistUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx);

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.fail({ message: "No checklist to delete", code: "NO_CHECKLIST" }, [
                Effects.toast("nothingToDelete", "warning"),
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Delete unavailable", code: "DELETE_UNAVAILABLE" }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationRuntime.OPERATIONS.DELETE
        }).then(function (oPermission) {
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to delete checklist", code: "NO_DELETE_PERMISSION" }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, "detailDeletePermissionDenied", [
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
                ]));
            }
            return Promise.resolve(oRepo.deleteChecklist({ rootId: sRootId })).then(function () {
                return Result.ok({ deleted: true, rootId: sRootId }, [
                    Effects.modelPatch("selected", "/", {}),
                    Effects.modelPatch("snapshot", "/", {}),
                    Effects.modelPatch("state", StatePaths.READINESS_DETAIL, {
                        status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE,
                        ready: false,
                        readyAt: "",
                        error: "",
                        rootId: "",
                        mode: WorkflowContracts.EDIT_MODES.READ,
                        permissionKnown: false,
                        lockKnown: false
                    }),
                    Effects.modelPatch("view", ViewPathContracts.ACCESS_STATE, DetailAuthorizationRuntime.buildAccessState({
                        rootId: "",
                        userId: "",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: "AUTHORIZED",
                        message: ""
                    }, false)),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                    Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch("state", ModelPathContracts.LOCK_OPERATION_PENDING, false),
                    Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch("state", ModelPathContracts.LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                    Effects.modelPatch("state", ModelPathContracts.ACTIVE_OBJECT_ID, null),
                    Effects.modelPatch("state", ModelPathContracts.SELECTED_ID, null),
                    Effects.modelPatch("state", ModelPathContracts.SEARCH_RETURN_CONTEXT, SearchReturnRediscoveryRuntime.buildContext({
                        rootId: sRootId,
                        reason: "detailDeleteCompleted",
                        mode: SearchReturnRediscoveryRuntime.MODES.DELETE,
                        focusRequested: false,
                        selectionRequested: false
                    })),
                    Effects.toast("checklistDeleted", "success"),
                    Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
                ]);
            });
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false)
            ]);
        });
    }

    return DeleteChecklistUseCase;
});
