sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeOverrideHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants"
], function (BehaviorRuntimeCore, BehaviorResolver, DetailRuntimeDefaultHandlers, DetailRuntimeOverrideHandlers, DetailRuntimeConstants) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: DetailRuntimeConstants.SCOPE,
        resolver: BehaviorResolver,
        defaultHandlers: DetailRuntimeDefaultHandlers,
        overrideHandlers: DetailRuntimeOverrideHandlers
    });
});
