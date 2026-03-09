sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/AccessDeniedControllerActions"
], function (BaseController, AccessDeniedControllerActions) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.AccessDenied", Object.assign({}, AccessDeniedControllerActions));
});
