sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaContracts, ModelPathContracts, ModelContracts) {
    "use strict";

    var READINESS_STATUS = ComponentDetailMetaContracts.READINESS_STATUS;
    var VALIDATION_STATE = ComponentDetailMetaContracts.VALIDATION_STATE;
    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;

    function syncDetailMeta(oStateModel, StatePaths) {
        var oReadiness = ModelStateRuntime.readOnModel(oStateModel, StatePaths.READINESS_DETAIL, {}) || {};
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var sAutosaveState = WorkflowContracts.normalizeAutosaveState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE));
        var oPersistence = ModelStateRuntime.readOnModel(oStateModel, StatePaths.PERSISTENCE, {}) || {};
        var sValidationSource = String((ModelStateRuntime.readOnModel(oStateModel, StatePaths.VALIDATION_SUMMARY, {}) || {}).source || VALIDATION_STATE.IDLE);
        var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
        var bPermissionKnown = !!oReadiness.permissionKnown;
        var sReadinessStatus = String(oReadiness.status || READINESS_STATUS.IDLE).trim() || READINESS_STATUS.IDLE;
        var bAllowed = bPermissionKnown && sReadinessStatus !== READINESS_STATUS.DENIED && sReadinessStatus !== READINESS_STATUS.ERROR;

        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {
            rootId: String(oReadiness.rootId || ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") || "").trim(),
            readiness: { status: sReadinessStatus, ready: !!oReadiness.ready, readyAt: String(oReadiness.readyAt || ""), error: String(oReadiness.error || "") },
            mode: sMode,
            lock: { state: sLockState, known: !!oReadiness.lockKnown },
            dirty: bDirty,
            permission: { known: bPermissionKnown, allowed: bAllowed },
            save: {
                state: String(oPersistence.state || sAutosaveState || "").trim() || sAutosaveState,
                messageKey: String(oPersistence.messageKey || "").trim(),
                lastSavedAt: oPersistence.lastSavedAt || ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null)
            },
            validation: { state: sValidationSource || VALIDATION_STATE.IDLE }
        });
    }

    function resetDetailAccessGuard(oStateModel) {
        ModelStateRuntime.writeOnModel(oStateModel, PATHS.DETAIL_ACCESS_GUARD, {
            rootId: "", userId: "", canView: true, canEdit: false, canDelete: false,
            reasonCode: VALUES.AUTHORIZED, message: "", checkedAt: ""
        });
    }

    function resetDetailNavigationState(oComponent) {
        ModelStateRuntime.resetDetailWorkflowState(oComponent, {
            [ModelPathContracts.SELECTED_ID]: "",
            [ModelPathContracts.ACTIVE_OBJECT_ID]: ""
        });
        ModelStateRuntime.write(oComponent, ComponentListenerContracts.MODEL_NAMES.SHELL, ModelContracts.MODEL_PATHS.SHELL_LAYOUT, VALUES.ONE_COLUMN);
        ModelStateRuntime.resetDetailRuntimeData(oComponent);
    }

    return {
        resetDetailAccessGuard: resetDetailAccessGuard,
        resetDetailNavigationState: resetDetailNavigationState,
        syncDetailMeta: syncDetailMeta
    };
});
