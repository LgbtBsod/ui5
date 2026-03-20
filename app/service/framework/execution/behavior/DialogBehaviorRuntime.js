sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DialogDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DialogOverrideHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, DialogDefaultHandlers, DialogOverrideHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "dialog",
        resolver: BehaviorResolver,
        defaultHandlers: DialogDefaultHandlers,
        overrideHandlers: DialogOverrideHandlers
    });
});
