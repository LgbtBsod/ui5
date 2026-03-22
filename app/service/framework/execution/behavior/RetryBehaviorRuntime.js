sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/RetryHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, RetryHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "retry",
        resolver: BehaviorResolver,
        defaultHandlers: RetryHandlers.defaults,
        overrideHandlers: RetryHandlers.overrides
    });
});
