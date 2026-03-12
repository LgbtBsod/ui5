sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitListenersRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailLifecycleRuntime"
], function (ComponentInitListenersRuntime, ComponentDetailLifecycleRuntime) {
    "use strict";

    return {
        attachInitListeners: ComponentInitListenersRuntime.attachInitListeners,
        shouldReleaseDetailLock: ComponentDetailLifecycleRuntime.shouldReleaseDetailLock,
        syncDetailMeta: ComponentDetailLifecycleRuntime.syncDetailMeta
    };
});
