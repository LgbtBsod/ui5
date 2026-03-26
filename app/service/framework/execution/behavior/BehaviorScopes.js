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
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
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
    BehaviorRegistry,
    UiBehaviorConstants,
    DetailRuntimeConstants
) {
    "use strict";

    var detailRuntimeDefaultsRegistered = false;
    var detailRuntimeHandlers = {};

    function resolveAnalyticsEditRestorePlan() {
        return {
            maxAttempts: 3,
            retryDelayMs: 220
        };
    }

    detailRuntimeHandlers[DetailRuntimeConstants.OP_ANALYTICS_EDIT_RESTORE] = resolveAnalyticsEditRestorePlan;

    function ensureDetailRuntimeRegistered() {
        if (detailRuntimeDefaultsRegistered) {
            return;
        }
        Object.keys(detailRuntimeHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(DetailRuntimeConstants.SCOPE, sOperation, detailRuntimeHandlers[sOperation]);
        });
        detailRuntimeDefaultsRegistered = true;
    }

    var detailRuntimeOverrides = {
        ensureRegistered: function () {
            return true;
        },
        register: function (sId, fnHandler) {
            return BehaviorRegistry.registerOverride(DetailRuntimeConstants.SCOPE, sId, fnHandler);
        },
        unregister: function (sId) {
            return BehaviorRegistry.unregisterOverride(DetailRuntimeConstants.SCOPE, sId);
        },
        clear: function () {
            return BehaviorRegistry.clearOverrides(DetailRuntimeConstants.SCOPE);
        }
    };

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
        detailRuntime: make(DetailRuntimeConstants.SCOPE, {
            handlers: detailRuntimeHandlers,
            ensureRegistered: ensureDetailRuntimeRegistered
        }, detailRuntimeOverrides)
    });
});
