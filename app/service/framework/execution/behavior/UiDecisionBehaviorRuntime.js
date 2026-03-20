sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, UiDecisionDefaultHandlers, UiDecisionOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "uiDecision",
        resolver: BehaviorResolver,
        defaultHandlers: UiDecisionDefaultHandlers,
        overrideHandlers: UiDecisionOverrideHandlers
    });
});
