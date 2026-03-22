sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants"
], function (BehaviorRuntimeCore, BehaviorResolver, DetailRuntimeHandlers, DetailRuntimeConstants) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: DetailRuntimeConstants.SCOPE,
        resolver: BehaviorResolver,
        defaultHandlers: DetailRuntimeHandlers.defaults,
        overrideHandlers: DetailRuntimeHandlers.overrides
    });
});
