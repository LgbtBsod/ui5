sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentManagerOrchestrationRuntime"
], function (ComponentLockEventsRuntime, ComponentManagerOrchestrationRuntime) {
    "use strict";

    return {
        attachLockRuntime: ComponentLockEventsRuntime.attachLockRuntime,
        attachManagerRuntime: ComponentManagerOrchestrationRuntime.attachManagerRuntime
    };
});
