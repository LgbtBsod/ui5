sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppOverlayBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppShellBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppStateBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/app/AppDomBehavior"
], function (
    BaseController,
    AppLifecycleBehavior,
    AppOverlayBehavior,
    AppShellBehavior,
    AppStateBehavior,
    AppDomBehavior
) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.App", Object.assign(
        {},
        AppLifecycleBehavior,
        AppOverlayBehavior,
        AppShellBehavior,
        AppStateBehavior,
        AppDomBehavior
    ));
});
