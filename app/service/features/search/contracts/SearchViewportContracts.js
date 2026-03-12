sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (ModelContracts) {
    "use strict";

    return Object.freeze({
        MODELS: Object.freeze({
            STATE: ModelContracts.MODELS.STATE
        }),
        STATE_PATHS: Object.freeze({
            SEARCH_SCROLL_STATE: "/searchScrollState"
        }),
        VIEW_PATHS: Object.freeze({
            SCROLL_NAV_VISIBLE: "/scrollNavVisible",
            RESULTS_TOOLBAR_NAV_VISIBLE: "/resultsToolbarNavVisible"
        })
    });
});
