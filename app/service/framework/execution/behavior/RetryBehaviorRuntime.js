sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/RetryDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/RetryOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, RetryDefaultHandlers, RetryOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "retry",
        resolver: BehaviorResolver,
        defaultHandlers: RetryDefaultHandlers,
        overrideHandlers: RetryOverrideHandlers
    });
});
