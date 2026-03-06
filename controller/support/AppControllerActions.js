sap.ui.define([
    "sap_ui5/controller/support/AppControllerLifecycleActions",
    "sap_ui5/controller/support/AppControllerOverlayActions",
    "sap_ui5/controller/support/AppControllerShellActions",
    "sap_ui5/controller/support/AppControllerStateActions",
    "sap_ui5/controller/support/AppControllerDomActions"
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
