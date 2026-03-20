sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorOverrideHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (BehaviorRuntimeCore, BehaviorResolver, UiBehaviorDefaultHandlers, UiBehaviorOverrideHandlers, UiBehaviorConstants) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: UiBehaviorConstants.SCOPE,
        resolver: BehaviorResolver,
        defaultHandlers: UiBehaviorDefaultHandlers,
        overrideHandlers: UiBehaviorOverrideHandlers
    });
});
