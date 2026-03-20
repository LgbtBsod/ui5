sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (ModelContracts) {
    "use strict";

    return Object.freeze({
        MODELS: Object.freeze({
            STATE: ModelContracts.MODELS.STATE
        }),
        STATE_PATHS: Object.freeze({
            SEARCH_SCROLL_STATE: "/searchScrollState"
        })
    });
});
