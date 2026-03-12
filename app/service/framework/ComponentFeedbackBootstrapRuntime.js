sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeSettingsBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentPendingNavigationRuntime"
], function (ComponentFeedbackRuntime, ComponentRuntimeSettingsBootstrap, ComponentPendingNavigationRuntime) {
    "use strict";

    function createRuntimeSettingsRuntime(oComponent, mOptions) {
        var oRuntime = ComponentRuntimeSettingsBootstrap.createRuntimeSettingsRuntime(oComponent, mOptions);

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
        createRuntimeSettingsRuntime: createRuntimeSettingsRuntime
    };
});
