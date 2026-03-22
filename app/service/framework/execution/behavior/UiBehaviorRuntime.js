sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (BehaviorRuntimeCore, BehaviorResolver, UiBehaviorHandlers, UiBehaviorConstants) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: UiBehaviorConstants.SCOPE,
        resolver: BehaviorResolver,
        defaultHandlers: UiBehaviorHandlers.defaults,
        overrideHandlers: UiBehaviorHandlers.overrides
    });
});
