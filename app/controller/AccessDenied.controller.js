sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/AccessDeniedControllerActions"
], function (BaseController, AccessDeniedControllerActions) {
    "use strict";

    return BaseController.extend("checklist.app.controller.AccessDenied", Object.assign({}, AccessDeniedControllerActions));
});
