sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeSettingsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentPendingNavigationRuntime"
], function (ComponentFeedbackRuntime, ComponentRuntimeSettingsRuntime, ComponentPendingNavigationRuntime) {
    "use strict";

    function initializeRuntimeSettings(oComponent, mOptions) {
        var oRuntime = ComponentRuntimeSettingsRuntime.initializeRuntimeSettings(oComponent, mOptions);

        return {
            applyRuntimeSettings: oRuntime.applyRuntimeSettings,
            loadRuntimeSettings: function (mLoadOptions) {
                return oRuntime.loadRuntimeSettings(mLoadOptions).catch(function (oError) {
                    throw oError || new Error("runtime_settings_load_failed");
                });
            }
        };
    }

    return {
        createFeedbackRuntime: ComponentFeedbackRuntime.createFeedbackRuntime,
        createPendingNavigationRuntime: ComponentPendingNavigationRuntime.createPendingNavigationRuntime,
            initializeRuntimeSettings: initializeRuntimeSettings
    };
});
