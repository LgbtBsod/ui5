sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/NavigationDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/NavigationOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, NavigationDefaultHandlers, NavigationOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "navigation",
        resolver: BehaviorResolver,
        defaultHandlers: NavigationDefaultHandlers,
        overrideHandlers: NavigationOverrideHandlers
    });
});
