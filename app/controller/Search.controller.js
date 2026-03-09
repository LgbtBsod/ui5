sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchControllerActions"
], function (BaseController, SearchControllerActions) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Search", Object.assign({}, SearchControllerActions));
});
