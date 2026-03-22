sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/OverrideHandlerFactory"
], function (BehaviorRuntimeCore, BehaviorResolver, UiDecisionDefaultHandlers, OverrideHandlerFactory) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "uiDecision",
        resolver: BehaviorResolver,
        defaultHandlers: UiDecisionDefaultHandlers,
        overrideHandlers: OverrideHandlerFactory.create("uiDecision")
    });
});
