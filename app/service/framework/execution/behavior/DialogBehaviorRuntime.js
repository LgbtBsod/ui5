sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorRuntimeCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DialogHandlers"
], function (BehaviorRuntimeCore, BehaviorResolver, DialogHandlers) {
    "use strict";

    return BehaviorRuntimeCore.create({
        scope: "dialog",
        resolver: BehaviorResolver,
        defaultHandlers: DialogHandlers.defaults,
        overrideHandlers: DialogHandlers.overrides
    });
});
