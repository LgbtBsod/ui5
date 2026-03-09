sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AnalyticsControllerActions"
], function (BaseController, AnalyticsControllerActions) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Analytics", Object.assign({}, AnalyticsControllerActions));
});
