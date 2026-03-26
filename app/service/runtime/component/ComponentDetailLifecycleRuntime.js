sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentNavigationGuardDecisionRuntime"
], function (ModelStateRuntime, ModelPathContracts, WorkflowContracts, ComponentNavigationGuardDecisionRuntime) {
    "use strict";

    function shouldReleaseDetailLock(oStateModel, oRouteEvent, StatePaths) {
        return ComponentNavigationGuardDecisionRuntime.shouldReleaseDetailLock(oStateModel, oRouteEvent, StatePaths);
    }

    function syncDetailMeta(oStateModel, StatePaths) {
        var oReadiness = ModelStateRuntime.readOnModel(oStateModel, StatePaths.READINESS_DETAIL, {}) || {};
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var sAutosaveState = WorkflowContracts.normalizeAutosaveState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE));
        var sValidationSource = String((ModelStateRuntime.readOnModel(oStateModel, StatePaths.VALIDATION_SUMMARY, {}) || {}).source || WorkflowContracts.VALIDATION_STATUS.IDLE);
        var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
        var bPermissionKnown = !!oReadiness.permissionKnown;
        var sReadinessStatus = String(oReadiness.status || WorkflowContracts.READINESS_STATUS.IDLE).trim() || WorkflowContracts.READINESS_STATUS.IDLE;
        var bAllowed = bPermissionKnown && sReadinessStatus !== WorkflowContracts.READINESS_STATUS.DENIED && sReadinessStatus !== WorkflowContracts.READINESS_STATUS.ERROR;
        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {
            rootId: String(oReadiness.rootId || ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim(),
            readiness: {
                status: sReadinessStatus,
                ready: !!oReadiness.ready,
                readyAt: String(oReadiness.readyAt || ""),
                error: String(oReadiness.error || "")
            },
            mode: sMode,
            lock: {
                state: sLockState,
                known: !!oReadiness.lockKnown
            },
            dirty: bDirty,
            permission: {
                known: bPermissionKnown,
                allowed: bAllowed
            },
            save: {
                state: sAutosaveState,
                lastSavedAt: ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null)
            },
            validation: {
                state: sValidationSource || WorkflowContracts.VALIDATION_STATUS.IDLE
            }
        });
    }

    return {
        shouldReleaseDetailLock: shouldReleaseDetailLock,
        syncDetailMeta: syncDetailMeta
    };
});
