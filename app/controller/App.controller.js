sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/AppControllerActions"
], function (BaseController, AppControllerActions) {
    "use strict";

    return BaseController.extend("checklist.app.controller.App", Object.assign({}, AppControllerActions));
});
