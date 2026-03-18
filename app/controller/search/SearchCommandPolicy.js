sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandPolicyRuntime"
], function (FacadeCommandRuntime, FacadeCommandPolicyRuntime) {
    "use strict";

    return FacadeCommandPolicyRuntime.buildPolicy({
        execute: function (oController, sMethod, mInput) {
            return FacadeCommandRuntime.executeSearch(
                oController,
                oController && oController._facade,
                sMethod,
                mInput || {}
            );
        },
        methods: [
            "buildFilter",
            "executeSearch",
            "rebind",
            "selectRow",
            "selectionChanged",
            "bootstrap",
            "analytics",
            "applyRebindPolicy",
            "exportFlow"
        ]
    });
});
