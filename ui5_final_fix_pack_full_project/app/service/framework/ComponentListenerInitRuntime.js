sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailMetaSyncRuntime"
], function (ModelStateRuntime, WorkflowContracts, ComponentListenerContracts, ComponentDetailMetaSyncRuntime) {
    "use strict";

    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;

    function initializeListeners(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oLayoutModel = mOptions.layoutModel;
        var SmartSearchAdapter = mOptions.smartSearchAdapter;

        mOptions.componentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
        ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, mOptions.statePaths || {});
        if (oComponent._fnOnFullSave) {
            window.removeEventListener(VALUES.FULL_SAVE_EVENT, oComponent._fnOnFullSave);
        }
        oComponent._fnOnFullSave = function () { oComponent._oGcd.resetOnFullSave(); };
        window.addEventListener(VALUES.FULL_SAVE_EVENT, oComponent._fnOnFullSave);
        oComponent.setModel(mOptions.layoutModel, ComponentListenerContracts.MODEL_NAMES.LAYOUT);
        oComponent.setModel(mOptions.cacheModel, ComponentListenerContracts.MODEL_NAMES.CACHE);
        oComponent.setModel(mOptions.masterDataModel, ComponentListenerContracts.MODEL_NAMES.MASTER_DATA);
        oComponent.setModel(mOptions.envModel, ComponentListenerContracts.MODEL_NAMES.ENV);
        if (oComponent._fnBeforeUnload) {
            window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
        }
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
        initializeListeners: initializeListeners
    };
});
