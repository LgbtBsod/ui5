sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/SearchUiConfig",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaSyncRuntime, DetailContracts, SearchUiConfig, ShellStateRuntime, ModelContracts) {
    "use strict";

    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;
    var PERSISTENCE_DIRTY_SOURCE_STATES = Object.freeze({
        [DetailContracts.STATES.IDLE]: true,
        [DetailContracts.STATES.SAVED]: true,
        [DetailContracts.STATES.DIRTY]: true
    });

    function syncDirtyPersistence(oStateModel, bDirty) {
        var sPersistenceState = String(ModelStateRuntime.readOnModel(oStateModel, "/persistence/state", "") || "").trim();
        if (!PERSISTENCE_DIRTY_SOURCE_STATES[sPersistenceState]) {
            return;
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/persistence/state", bDirty ? DetailContracts.STATES.DIRTY : DetailContracts.STATES.IDLE);
        ModelStateRuntime.writeOnModel(oStateModel, "/persistence/messageKey", bDirty ? DetailContracts.PERSISTENCE_DIRTY : DetailContracts.PERSISTENCE_IDLE);
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

    function replaceWindowListener(oComponent, sListenerKey, sEventName, fnListener) {
        if (oComponent[sListenerKey]) {
            window.removeEventListener(sEventName, oComponent[sListenerKey]);
        }
        oComponent[sListenerKey] = fnListener;
        window.addEventListener(sEventName, fnListener);
    }

    function seedShellRuntimeState(oShellModel, oSearchConfig) {
        ModelStateRuntime.setManyOnModel(oShellModel, {
            [MODEL_PATHS.SHELL_LAYOUT]: ModelStateRuntime.readOnModel(oShellModel, MODEL_PATHS.SHELL_LAYOUT, VALUES.ONE_COLUMN) || VALUES.ONE_COLUMN,
            [MODEL_PATHS.SHELL_SMART_FILTER_FIELDS]: oSearchConfig.smartFilter.fields,
            [MODEL_PATHS.SHELL_SMART_TABLE_COLUMNS]: oSearchConfig.smartTable.columns,
            [MODEL_PATHS.SHELL_SMART_TABLE_SELECTION_MODE]: oSearchConfig.smartTable.selectionMode
        });
    }

    function attachDirtyStateBinding(oComponent, oStateModel) {
        if (oComponent._oDirtyStateBinding && oComponent._fnDirtyStateBindingChange) {
            oComponent._oDirtyStateBinding.detachChange(oComponent._fnDirtyStateBindingChange);
        }
        oComponent._oDirtyStateBinding = oStateModel.bindProperty(PATHS.IS_DIRTY);
        oComponent._fnDirtyStateBindingChange = function () {
            var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false);
            syncDirtyPersistence(oStateModel, bDirty);
            oComponent._oAutoSave.touch();
        };
        oComponent._oDirtyStateBinding.attachChange(oComponent._fnDirtyStateBindingChange);
    }

    function attachLockScopedStateBindings(oComponent, oStateModel, mOptions) {
        (oComponent._aLockScopedStateBindings || []).forEach(function (oEntry) {
            if (oEntry && oEntry.binding && oEntry.handler) {
                oEntry.binding.detachChange(oEntry.handler);
            }
        });
        oComponent._aLockScopedStateBindings = [mOptions.statePaths.WORKFLOW_DETAIL_LOCK_STATE, mOptions.statePaths.WORKFLOW_DETAIL_EDIT_MODE].map(function (sPath) {
            var oBinding = oStateModel.bindProperty(sPath);
            var fnBindingChange = function () { oComponent._syncLockScopedManagers(oStateModel); };
            oBinding.attachChange(fnBindingChange);
            return { binding: oBinding, handler: fnBindingChange };
        });
    }

    function initializeListeners(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var oSearchConfig = mOptions.searchConfig || SearchUiConfig.getLayoutSeed();

        ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel);
        ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, mOptions.statePaths || {});
        replaceWindowListener(oComponent, "_fnOnFullSave", VALUES.FULL_SAVE_EVENT, function () {
            oComponent._oGcd.resetOnFullSave();
        });
        oComponent.setModel(mOptions.shellModel, ComponentListenerContracts.MODEL_NAMES.SHELL);
        oComponent.setModel(mOptions.masterDataModel, ComponentListenerContracts.MODEL_NAMES.MASTER_DATA);
        replaceWindowListener(oComponent, "_fnBeforeUnload", "beforeunload", createBeforeUnloadHandler(oStateModel, mOptions));
        seedShellRuntimeState(oShellModel, oSearchConfig);
        attachDirtyStateBinding(oComponent, oStateModel);
        attachLockScopedStateBindings(oComponent, oStateModel, mOptions);
    }

    return {
        attachDirtyStateBinding: attachDirtyStateBinding,
        attachLockScopedStateBindings: attachLockScopedStateBindings,
        createBeforeUnloadHandler: createBeforeUnloadHandler,
        initializeListeners: initializeListeners,
        replaceWindowListener: replaceWindowListener,
        seedShellRuntimeState: seedShellRuntimeState
    };
});
