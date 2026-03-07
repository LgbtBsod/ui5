sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap_ui5/controller/support/AnalyticsControllerActions"
], function (BaseController, AnalyticsControllerActions) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.Analytics", Object.assign({}, AnalyticsControllerActions));
});
