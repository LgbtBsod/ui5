sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailPersistenceConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/SearchUiConfig",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaSyncRuntime, WorkflowRuntimeConstants, DetailPersistenceConstants, SearchUiConfig, ModelContracts) {
    "use strict";

    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;
    var PERSISTENCE_DIRTY_SOURCE_STATES = Object.freeze({
        idle: true,
        saved: true,
        dirty: true
    });

    function syncDirtyPersistence(oStateModel, bDirty) {
        var sPersistenceState = String(ModelStateRuntime.readOnModel(oStateModel, "/persistence/state", "") || "").trim();
        if (!PERSISTENCE_DIRTY_SOURCE_STATES[sPersistenceState]) {
            return;
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/persistence/state", bDirty ? DetailPersistenceConstants.STATES.DIRTY : DetailPersistenceConstants.STATES.IDLE);
        ModelStateRuntime.writeOnModel(oStateModel, "/persistence/messageKey", bDirty ? "persistenceDirty" : "persistenceIdle");
    }

    function createBeforeUnloadHandler(oStateModel, mOptions) {
        return function (oEvent) {
            var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, mOptions.statePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
            var bHasUnsaved = WorkflowContracts.isEditableMode(sMode) && ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false);
            if (!bHasUnsaved) {
                return;
            }
            oEvent.preventDefault();
            oEvent.returnValue = VALUES.UNSAVED_CHANGES_MESSAGE;
            return VALUES.UNSAVED_CHANGES_MESSAGE;
        };
    }

    function initializeListeners(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var oSearchConfig = mOptions.searchConfig || SearchUiConfig.getLayoutSeed();

        mOptions.componentRuntimeSupport.syncShellRuntimeState(oStateModel, oShellModel);
        ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, mOptions.statePaths || {});
        if (oComponent._fnOnFullSave) {
            window.removeEventListener(VALUES.FULL_SAVE_EVENT, oComponent._fnOnFullSave);
        }
        oComponent._fnOnFullSave = function () { oComponent._oGcd.resetOnFullSave(); };
        window.addEventListener(VALUES.FULL_SAVE_EVENT, oComponent._fnOnFullSave);
        oComponent.setModel(mOptions.shellModel, ComponentListenerContracts.MODEL_NAMES.SHELL);
        oComponent.setModel(mOptions.cacheModel, ComponentListenerContracts.MODEL_NAMES.CACHE);
        oComponent.setModel(mOptions.masterDataModel, ComponentListenerContracts.MODEL_NAMES.MASTER_DATA);
        oComponent.setModel(mOptions.envModel, ComponentListenerContracts.MODEL_NAMES.ENV);
        if (oComponent._fnBeforeUnload) {
            window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
        }
        oComponent._fnBeforeUnload = createBeforeUnloadHandler(oStateModel, mOptions);
        window.addEventListener("beforeunload", oComponent._fnBeforeUnload);
        ModelStateRuntime.setManyOnModel(oShellModel, {
            [MODEL_PATHS.SHELL_SMART_FILTER_FIELDS]: oSearchConfig.smartFilter.fields,
            [MODEL_PATHS.SHELL_SMART_TABLE_COLUMNS]: oSearchConfig.smartTable.columns,
            [MODEL_PATHS.SHELL_SMART_TABLE_SELECTION_MODE]: oSearchConfig.smartTable.selectionMode
        });
        oComponent._oDirtyStateBinding = oStateModel.bindProperty(PATHS.IS_DIRTY);
        oComponent._fnDirtyStateBindingChange = function () {
            var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false);
            syncDirtyPersistence(oStateModel, bDirty);
            oComponent._oAutoSave.touch();
        };
        oComponent._oDirtyStateBinding.attachChange(oComponent._fnDirtyStateBindingChange);
        oComponent._aLockScopedStateBindings = [mOptions.statePaths.WORKFLOW_DETAIL_LOCK_STATE, mOptions.statePaths.WORKFLOW_DETAIL_EDIT_MODE].map(function (sPath) {
            var oBinding = oStateModel.bindProperty(sPath);
            var fnBindingChange = function () { oComponent._syncLockScopedManagers(oStateModel); };
            oBinding.attachChange(fnBindingChange);
            return { binding: oBinding, handler: fnBindingChange };
        });
    }

    return {
        createBeforeUnloadHandler: createBeforeUnloadHandler,
        initializeListeners: initializeListeners
    };
});
