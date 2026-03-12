sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailMetaContracts"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaContracts) {
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
            save: { state: sAutosaveState, lastSavedAt: ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null) },
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
            "/selectedId": "",
            "/activeObjectId": "",
            "/layout": VALUES.ONE_COLUMN
        });
        ModelStateRuntime.resetDetailRuntimeData(oComponent);
    }

    function attachLifecycleBindings(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oSelectedModel = mOptions.selectedModel;
        var StatePaths = mOptions.statePaths || {};
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnPublishTabSignal = mOptions.publishTabSignal;

        oComponent._oStateLifecycleModel = oStateModel;
        oComponent._oSelectedLifecycleModel = oSelectedModel;
        oComponent._fnStateModelPropertyChange = function (oEvent) {
            var sPath = oEvent.getParameter("path") || "";
            if ([PATHS.IS_LOADING, PATHS.ACTIVE_OBJECT_ID, StatePaths.SESSION_ID, StatePaths.UI_BUSY_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE].indexOf(sPath) >= 0) {
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
            }
            if ([PATHS.ACTIVE_OBJECT_ID, StatePaths.READINESS_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, StatePaths.WORKFLOW_DIRTY, StatePaths.VALIDATION_SUMMARY].indexOf(sPath) >= 0) {
                syncDetailMeta(oStateModel, StatePaths);
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                fnEmitTelemetry("workflow.mode.changed", mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                fnEmitTelemetry("lock.state.changed", mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if ([StatePaths.SAVE_IN_FLIGHT, StatePaths.WORKFLOW_DIRTY].indexOf(sPath) >= 0 &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false) &&
                !ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false) &&
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, null)) {
                mOptions.resumePendingNavigationIntent();
            }
            if ([StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, PATHS.ACTIVE_OBJECT_ID].indexOf(sPath) >= 0) {
                var sCurrentRootId = String(ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") || "").trim();
                var sCurrentMode = mOptions.layoutStateRuntime.readMode(oStateModel, "");
                var sCurrentLockState = mOptions.layoutStateRuntime.readLockState(oStateModel, "");
                if (sCurrentRootId && sCurrentMode === WorkflowContracts.EDIT_MODES.EDIT && sCurrentLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
                    fnPublishTabSignal(VALUES.LOCK_OWNED, { rootId: sCurrentRootId });
                } else if (sCurrentRootId && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sCurrentMode !== WorkflowContracts.EDIT_MODES.EDIT) {
                    fnPublishTabSignal(VALUES.LOCK_RELEASED, { rootId: sCurrentRootId });
                }
            }
        };
        oComponent._fnSelectedModelPropertyChange = function () { return; };
        oStateModel.attachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
        oSelectedModel.attachPropertyChange(oComponent._fnSelectedModelPropertyChange, oComponent);
        oComponent._detachInitRuntimeListeners = function () {
            if (oComponent._oStateLifecycleModel && oComponent._fnStateModelPropertyChange) {
                oComponent._oStateLifecycleModel.detachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
            }
            if (oComponent._oSelectedLifecycleModel && oComponent._fnSelectedModelPropertyChange) {
                oComponent._oSelectedLifecycleModel.detachPropertyChange(oComponent._fnSelectedModelPropertyChange, oComponent);
            }
            if (oComponent._fnBeforeUnload) {
                window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
            }
        };
    }

    function initializeListenerState(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oLayoutModel = mOptions.layoutModel;
        var SmartSearchAdapter = mOptions.smartSearchAdapter;

        mOptions.componentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
        syncDetailMeta(oStateModel, mOptions.statePaths || {});
        oComponent._fnOnFullSave = function () { oComponent._oGcd.resetOnFullSave(); };
        window.addEventListener(VALUES.FULL_SAVE_EVENT, oComponent._fnOnFullSave);
        oComponent.setModel(mOptions.layoutModel, ComponentListenerContracts.MODEL_NAMES.LAYOUT);
        oComponent.setModel(mOptions.cacheModel, ComponentListenerContracts.MODEL_NAMES.CACHE);
        oComponent.setModel(mOptions.masterDataModel, ComponentListenerContracts.MODEL_NAMES.MASTER_DATA);
        oComponent.setModel(mOptions.envModel, ComponentListenerContracts.MODEL_NAMES.ENV);
        oComponent._fnBeforeUnload = function (oEvent) {
            var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, mOptions.statePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
            var bHasUnsaved = WorkflowContracts.isEditableMode(sMode) && ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false);
            if (!bHasUnsaved) {
                return;
            }
            oEvent.preventDefault();
            oEvent.returnValue = VALUES.UNSAVED_CHANGES_MESSAGE;
            return VALUES.UNSAVED_CHANGES_MESSAGE;
        };
        window.addEventListener("beforeunload", oComponent._fnBeforeUnload);
        ModelStateRuntime.setManyOnModel(oLayoutModel, {
            "/smartFilter/fields": SmartSearchAdapter.getSmartFilterConfig().fields,
            "/smartTable/columns": SmartSearchAdapter.getSmartTableConfig().columns,
            "/smartTable/selectionMode": SmartSearchAdapter.getSmartTableConfig().selectionMode
        });
        oComponent._oDirtyStateBinding = oStateModel.bindProperty(PATHS.IS_DIRTY);
        oComponent._fnDirtyStateBindingChange = function () { oComponent._oAutoSave.touch(); };
        oComponent._oDirtyStateBinding.attachChange(oComponent._fnDirtyStateBindingChange);
        oComponent._aLockScopedStateBindings = [mOptions.statePaths.WORKFLOW_DETAIL_LOCK_STATE, mOptions.statePaths.WORKFLOW_DETAIL_EDIT_MODE].map(function (sPath) {
            var oBinding = oStateModel.bindProperty(sPath);
            var fnBindingChange = function () { oComponent._syncLockScopedManagers(oStateModel); };
            oBinding.attachChange(fnBindingChange);
            return { binding: oBinding, handler: fnBindingChange };
        });
    }

    return {
        attachLifecycleBindings: attachLifecycleBindings,
        initializeListenerState: initializeListenerState,
        resetDetailAccessGuard: resetDetailAccessGuard,
        resetDetailNavigationState: resetDetailNavigationState,
        syncDetailMeta: syncDetailMeta
    };
});
