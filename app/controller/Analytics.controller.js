sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsControllerBehavior"
], function (BaseController, AnalyticsControllerBehavior) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Analytics", Object.assign({}, AnalyticsControllerBehavior));
});
