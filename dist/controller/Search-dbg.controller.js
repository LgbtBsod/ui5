sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchControllerBehavior"
], function (BaseController, SearchControllerBehavior) {
    "use strict";

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Search", Object.assign({}, SearchControllerBehavior));
});
