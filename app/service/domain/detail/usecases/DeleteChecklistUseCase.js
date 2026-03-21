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
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailUseCaseConstants"
], function (Result, Effects, DetailAuthorizationRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, ViewPathContracts, ModelPathContracts, NavigationContracts, WorkflowContracts, WorkflowRuntimeConstants, SearchReturnRediscoveryRuntime, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;
    var SNAPSHOT_MODEL = MODELS.SNAPSHOT;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_ACCESS_REASON_CODES = DetailUseCaseConstants.ACCESS_REASON_CODES;
    var DETAIL_CODES = DetailUseCaseConstants.CODES;
    var DETAIL_MESSAGE_KEYS = DetailUseCaseConstants.MESSAGE_KEYS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;
    var DETAIL_REASONS = DetailUseCaseConstants.REASONS;

    function DeleteChecklistUseCase() {
        return {
            execute: execute
        };
    }

    function execute(mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = DetailRuntimePayload.rootId(mInput, mCtx);

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.fail({ message: "No checklist to delete", code: DETAIL_CODES.NO_CHECKLIST }, [
                Effects.toast(DETAIL_MESSAGE_KEYS.NOTHING_TO_DELETE, "warning"),
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
            return Promise.resolve(Result.fail({ message: "Delete unavailable", code: DETAIL_CODES.DELETE_UNAVAILABLE }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sRootId, {
            activity: DetailAuthorizationRuntime.OPERATIONS.DELETE
        }).then(function (oPermission) {
            if (!oPermission.allowed) {
                return Result.fail({ message: "No permission to delete checklist", code: DETAIL_CODES.NO_DELETE_PERMISSION }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, [
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
                ]));
            }
            return Promise.resolve(oRepo.deleteChecklist({ rootId: sRootId })).then(function () {
                return Result.ok({ deleted: true, rootId: sRootId }, [
                    Effects.modelPatch(SELECTED_MODEL, DETAIL_MODEL_PATHS.ROOT, {}),
                    Effects.modelPatch(SNAPSHOT_MODEL, DETAIL_MODEL_PATHS.ROOT, {}),
                    Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                        status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE,
                        ready: false,
                        readyAt: "",
                        error: "",
                        rootId: "",
                        mode: WorkflowContracts.EDIT_MODES.READ,
                        permissionKnown: false,
                        lockKnown: false
                    }),
                    Effects.modelPatch(VIEW_MODEL, ViewPathContracts.ACCESS_STATE, DetailAuthorizationRuntime.buildAccessState({
                        rootId: "",
                        userId: "",
                        canView: true,
                        canEdit: true,
                        canDelete: true,
                        reasonCode: DETAIL_ACCESS_REASON_CODES.AUTHORIZED,
                        message: ""
                    }, false)),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.IDLE),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
                    Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
                    Effects.modelPatch(STATE_MODEL, ModelPathContracts.LOCK_OPERATION_PENDING, false),
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch(STATE_MODEL, ModelPathContracts.LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                    Effects.modelPatch(STATE_MODEL, ModelPathContracts.ACTIVE_OBJECT_ID, null),
                    Effects.modelPatch(STATE_MODEL, ModelPathContracts.SELECTED_ID, null),
                    Effects.modelPatch(STATE_MODEL, ModelPathContracts.SEARCH_RETURN_CONTEXT, SearchReturnRediscoveryRuntime.buildContext({
                        rootId: sRootId,
                        reason: DETAIL_REASONS.DETAIL_DELETE_COMPLETED,
                        mode: SearchReturnRediscoveryRuntime.MODES.DELETE,
                        focusRequested: false,
                        selectionRequested: false
                    })),
                    Effects.toast(DETAIL_MESSAGE_KEYS.CHECKLIST_DELETED, "success"),
                    Effects.navigate(NavigationContracts.ROUTES.SEARCH, {}, true)
                ]);
            });
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ]);
        });
    }

    return DeleteChecklistUseCase;
});
