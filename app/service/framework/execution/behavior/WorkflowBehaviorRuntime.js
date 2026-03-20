sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, WorkflowDefaultHandlers, WorkflowOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "workflow",
        resolver: BehaviorResolver,
        defaultHandlers: WorkflowDefaultHandlers,
        overrideHandlers: WorkflowOverrideHandlers
    });
});
