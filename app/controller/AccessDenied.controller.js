sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap_ui5/controller/support/AccessDeniedControllerActions"
], function (BaseController, AccessDeniedControllerActions) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.AccessDenied", Object.assign({}, AccessDeniedControllerActions));
});
