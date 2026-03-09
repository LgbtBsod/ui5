sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppControllerLifecycleActions",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppControllerOverlayActions",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppControllerShellActions",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppControllerStateRuntimeActions",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AppControllerDomActions"
], function (
    BaseController,
    AppControllerLifecycleActions,
    AppControllerOverlayActions,
    AppControllerShellActions,
    AppControllerStateRuntimeActions,
    AppControllerDomActions
) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.App", Object.assign(
        {},
        AppControllerLifecycleActions,
        AppControllerOverlayActions,
        AppControllerShellActions,
        AppControllerStateRuntimeActions,
        AppControllerDomActions
    ));
});
