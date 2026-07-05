sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAuthorizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (Result, Effects, DetailAuthorizationRuntime, DetailRuntimePayload, StatePaths, CreateSentinel, ViewPathContracts, NavigationContracts, WorkflowContracts, SearchReturnRediscoveryRuntime, ModelContracts, DetailContracts, MessageCodeConstants, MessageKeyConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MODEL = MODELS.DETAIL;
    var SHELL_MODEL = MODELS.SHELL;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_ACCESS_REASON_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
    var DETAIL_VIEW_KEYS = MessageKeyConstants.VIEW;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_REASONS = DetailContracts.REASONS;

    function DeleteChecklistUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function execute(mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sDbKey = DetailRuntimePayload.dbKey(mInput, mCtx);

        if (!sDbKey || CreateSentinel.isCreateId(sDbKey)) {
            return Promise.resolve(Result.fail({ messageKey: DETAIL_VIEW_KEYS.NOTHING_TO_DELETE, code: DETAIL_CODES.NO_CHECKLIST }, [
                Effects.toast(DETAIL_VIEW_KEYS.NOTHING_TO_DELETE, "warning"),
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        if (!oRepo || typeof oRepo.deleteChecklist !== "function") {
            return Promise.resolve(Result.fail({ messageKey: DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, code: DETAIL_CODES.DELETE_UNAVAILABLE }, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
            ]));
        }

        return DetailAuthorizationRuntime.fetchPermission(mCtx || {}, sDbKey, {
            activity: DetailAuthorizationRuntime.OPERATIONS.DELETE
        }).then(function (oPermission) {
            if (!oPermission.allowed) {
                return Result.fail({ messageKey: DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, code: DETAIL_CODES.NO_DELETE_PERMISSION }, DetailAuthorizationRuntime.deniedActionEffects(oPermission, DETAIL_MESSAGE_KEYS.DETAIL_DELETE_PERMISSION_DENIED, [
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false)
                ]));
            }
            return Promise.resolve(oRepo.deleteChecklist({ dbKey: sDbKey })).then(function () {
                return Result.ok({ deleted: true, dbKey: sDbKey }, [
                    Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.ROOT, {}),
                    Effects.modelPatch(DETAIL_MODEL, DETAIL_MODEL_PATHS.BASE, {}),
                    Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_DETAIL, {
                        status: WorkflowContracts.READINESS_STATUS.IDLE,
                        ready: false,
                        readyAt: "",
                        error: "",
                        dbKey: "",
                        rootId: "",
                        mode: WorkflowContracts.EDIT_MODES.READ,
                        permissionKnown: false,
                        lockKnown: false
                    }),
                    Effects.modelPatch(VIEW_MODEL, ViewPathContracts.ACCESS_STATE, DetailAuthorizationRuntime.buildAccessState({
                        dbKey: "",
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
                    Effects.modelPatch(STATE_MODEL, StatePaths.LOCK_OPERATION_PENDING, false),
                    Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_DETAIL, false),
                    Effects.modelPatch(SHELL_MODEL, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN),
                    Effects.modelPatch(STATE_MODEL, StatePaths.ACTIVE_OBJECT_ID, null),
                    Effects.modelPatch(STATE_MODEL, StatePaths.SELECTED_ID, null),
                    Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_RETURN_CONTEXT, SearchReturnRediscoveryRuntime.buildContext({
                        dbKey: sDbKey,
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
