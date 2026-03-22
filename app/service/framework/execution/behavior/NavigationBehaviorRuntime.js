sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/NavigationDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/OverrideHandlerFactory"
], function (BehaviorRuntimeCore, BehaviorResolver, NavigationDefaultHandlers, OverrideHandlerFactory) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "navigation",
        resolver: BehaviorResolver,
        defaultHandlers: NavigationDefaultHandlers,
        overrideHandlers: OverrideHandlerFactory.create("navigation")
    });
});
