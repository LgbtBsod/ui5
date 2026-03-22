sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/OverrideHandlerFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/NavigationDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DialogHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/RetryHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants"
], function (
    BehaviorRuntimeCore,
    BehaviorResolver,
    OverrideHandlerFactory,
    WorkflowDefaultHandlers,
    NavigationDefaultHandlers,
    DialogHandlers,
    FeedbackDefaultHandlers,
    RetryHandlers,
    UiBehaviorHandlers,
    UiDecisionDefaultHandlers,
    DetailRuntimeHandlers,
    UiBehaviorConstants,
    DetailRuntimeConstants
) {
    "use strict";

    function make(sScope, oHandlers, oOverrides) {
        return BehaviorRuntimeCore.create({
            scope: sScope,
            resolver: BehaviorResolver,
            defaultHandlers: oHandlers,
            overrideHandlers: oOverrides || OverrideHandlerFactory.create(sScope)
        });
    }

    return Object.freeze({
        workflow: make("workflow", WorkflowDefaultHandlers),
        navigation: make("navigation", NavigationDefaultHandlers),
        dialog: make("dialog", DialogHandlers.defaults, DialogHandlers.overrides),
        feedback: make("feedback", FeedbackDefaultHandlers),
        retry: make("retry", RetryHandlers.defaults, RetryHandlers.overrides),
        ui: make(UiBehaviorConstants.SCOPE, UiBehaviorHandlers.defaults, UiBehaviorHandlers.overrides),
        uiDecision: make("uiDecision", UiDecisionDefaultHandlers),
        detailRuntime: make(DetailRuntimeConstants.SCOPE, DetailRuntimeHandlers.defaults, DetailRuntimeHandlers.overrides)
    });
});
