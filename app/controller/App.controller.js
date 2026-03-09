sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/AppControllerLifecycleActions",
    "checklist/app/controller/support/AppControllerOverlayActions",
    "checklist/app/controller/support/AppControllerShellActions",
    "checklist/app/controller/support/AppControllerStateRuntimeActions",
    "checklist/app/controller/support/AppControllerDomActions"
], function (
    BaseController,
    AppControllerLifecycleActions,
    AppControllerOverlayActions,
    AppControllerShellActions,
    AppControllerStateRuntimeActions,
    AppControllerDomActions
) {
    "use strict";

    return BaseController.extend("checklist.app.controller.App", Object.assign(
        {},
        AppControllerLifecycleActions,
        AppControllerOverlayActions,
        AppControllerShellActions,
        AppControllerStateRuntimeActions,
        AppControllerDomActions
    ));
});
