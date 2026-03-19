sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsControllerRuntime"
], function (BaseController, AnalyticsControllerRuntime) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Analytics", AnalyticsControllerRuntime);
});
