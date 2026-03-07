sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap_ui5/controller/support/AppControllerActions"
], function (BaseController, AppControllerActions) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", Object.assign({}, AppControllerActions));
});
