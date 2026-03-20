sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, FeedbackDefaultHandlers, FeedbackOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "feedback",
        resolver: BehaviorResolver,
        defaultHandlers: FeedbackDefaultHandlers,
        overrideHandlers: FeedbackOverrideHandlers
    });
});
