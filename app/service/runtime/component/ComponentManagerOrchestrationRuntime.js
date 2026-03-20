sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentAutosaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentPollingRuntime"
], function (ComponentAutosaveRuntime, ComponentPollingRuntime) {
    "use strict";

    function attachManagerRuntime(mOptions) {
        ComponentPollingRuntime.createHeartbeatManager(mOptions);
        ComponentPollingRuntime.createSupportManagers(mOptions);
        ComponentAutosaveRuntime.createAutoSaveManager(mOptions);
        ComponentPollingRuntime.createLockStatusManager(mOptions);
    }

    return {
        attachManagerRuntime: attachManagerRuntime
    };
});
