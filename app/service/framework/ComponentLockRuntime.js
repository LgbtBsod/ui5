sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCoordinatorRuntime"
], function (ComponentCoordinatorRuntime) {
    "use strict";

    return {
        attachLockRuntime: ComponentCoordinatorRuntime.attachLockRuntime,
        attachManagerRuntime: ComponentCoordinatorRuntime.attachManagerRuntime
    };
});
