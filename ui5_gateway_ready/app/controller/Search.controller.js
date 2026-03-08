sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/SearchControllerActions"
], function (BaseController, SearchControllerActions) {
    "use strict";

    return BaseController.extend("checklist.app.controller.Search", Object.assign({}, SearchControllerActions));
});
