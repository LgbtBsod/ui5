sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (ModelStateRuntime, StatePaths, WorkflowContracts, WorkflowRuntimeConstants) {
    "use strict";

    function sync(oStateModel) {
        if (!oStateModel || !oStateModel.getProperty || !oStateModel.setProperty) {
            return null;
        }
        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {
            rootId: String(oStateModel.getProperty("/activeObjectId") || "").trim(),
            readiness: oStateModel.getProperty(StatePaths.READINESS_DETAIL) || {
                status: WorkflowRuntimeConstants.READINESS_STATUS.IDLE,
                ready: false,
                readyAt: "",
                error: ""
            },
            mode: oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_EDIT_MODE) || WorkflowContracts.EDIT_MODES.READ,
            lock: {
                state: oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_LOCK_STATE) || WorkflowContracts.LOCK_STATES.READ_ONLY,
                known: !!oStateModel.getProperty(StatePaths.PERSISTENCE_HAS_VALID_LOCK)
            },
            dirty: !!oStateModel.getProperty(StatePaths.WORKFLOW_DIRTY),
            permission: {
                known: !!((oStateModel.getProperty(StatePaths.READINESS_DETAIL) || {}).permissionKnown),
                allowed: ((oStateModel.getProperty(StatePaths.READINESS_DETAIL) || {}).status) !== WorkflowRuntimeConstants.READINESS_STATUS.DENIED
            },
            save: {
                state: oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE) || WorkflowContracts.AUTOSAVE_STATES.IDLE,
                lastSavedAt: oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT) || null
            },
            validation: {
                state: ((oStateModel.getProperty(StatePaths.VALIDATION_SUMMARY) || {}).hasErrors)
                    ? WorkflowRuntimeConstants.VALIDATION_STATUS.INVALID
                    : WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE
            }
        });
        return oStateModel.getProperty(StatePaths.DETAIL_META);
    }

    return {
        sync: sync
    };
});
