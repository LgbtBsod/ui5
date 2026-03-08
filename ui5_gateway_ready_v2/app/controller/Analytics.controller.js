sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/AnalyticsControllerActions"
], function (BaseController, AnalyticsControllerActions) {
    "use strict";

    return BaseController.extend("checklist.app.controller.Analytics", Object.assign({}, AnalyticsControllerActions));
});
