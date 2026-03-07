sap.ui.define([
    "checklist/app/controller/support/AppControllerLifecycleActions",
    "checklist/app/controller/support/AppControllerOverlayActions",
    "checklist/app/controller/support/AppControllerShellActions",
    "checklist/app/controller/support/AppControllerStateActions",
    "checklist/app/controller/support/AppControllerDomActions"
], function (
    AppControllerLifecycleActions,
    AppControllerOverlayActions,
    AppControllerShellActions,
    AppControllerStateActions,
    AppControllerDomActions
) {
    "use strict";

    return Object.assign({},
        AppControllerLifecycleActions,
        AppControllerOverlayActions,
        AppControllerShellActions,
        AppControllerStateActions,
        AppControllerDomActions
    );
});
