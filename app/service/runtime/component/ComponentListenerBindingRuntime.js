sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaSyncRuntime, ShellStateRuntime) {
    "use strict";

    var PATHS = ComponentListenerContracts.PATHS;
    var TELEMETRY_EVENT = ComponentListenerContracts.TELEMETRY_EVENT;
    var VALUES = ComponentListenerContracts.VALUES;

    function attachLifecycleBindings(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var StatePaths = mOptions.statePaths || {};
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnPublishTabSignal = mOptions.publishTabSignal;

        oComponent._oStateLifecycleModel = oStateModel;
        oComponent._fnStateModelPropertyChange = function (oEvent) {
            var sCurrentRootId;
            var sCurrentMode;
            var sCurrentLockState;
            var sPath = oEvent.getParameter("path") || "";
            if ([PATHS.IS_LOADING, PATHS.ACTIVE_OBJECT_ID, StatePaths.SESSION_ID, StatePaths.UI_BUSY_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE].indexOf(sPath) >= 0) {
                ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel);
            }
            if ([PATHS.ACTIVE_OBJECT_ID, StatePaths.READINESS_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, StatePaths.WORKFLOW_DIRTY, StatePaths.VALIDATION_SUMMARY].indexOf(sPath) >= 0) {
                ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, StatePaths);
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                fnEmitTelemetry(TELEMETRY_EVENT.WORKFLOW_MODE_CHANGED, mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                fnEmitTelemetry(TELEMETRY_EVENT.LOCK_STATE_CHANGED, mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if ([StatePaths.SAVE_IN_FLIGHT, StatePaths.WORKFLOW_DIRTY].indexOf(sPath) >= 0 &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false) &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false) &&
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, null)) {
                mOptions.resumePendingNavigationIntent();
            }
            if ([StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, PATHS.ACTIVE_OBJECT_ID].indexOf(sPath) >= 0) {
                sCurrentRootId = String(ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") || "").trim();
                sCurrentMode = mOptions.layoutStateRuntime.readMode(oStateModel, "");
                sCurrentLockState = mOptions.layoutStateRuntime.readLockState(oStateModel, "");
                if (sCurrentRootId && sCurrentMode === WorkflowContracts.EDIT_MODES.EDIT && sCurrentLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
                    fnPublishTabSignal(VALUES.LOCK_OWNED, { rootId: sCurrentRootId });
                } else if (sCurrentRootId && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sCurrentMode !== WorkflowContracts.EDIT_MODES.EDIT) {
                    fnPublishTabSignal(VALUES.LOCK_RELEASED, { rootId: sCurrentRootId });
                }
            }
        };
        oStateModel.attachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
        oComponent._detachInitRuntimeListeners = function () {
            if (oComponent._oStateLifecycleModel && oComponent._fnStateModelPropertyChange) {
                oComponent._oStateLifecycleModel.detachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
            }
            if (oComponent._fnBeforeUnload) {
                window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
            }
        };
    }

    return {
        attachLifecycleBindings: attachLifecycleBindings
    };
});
