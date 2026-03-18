sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandPolicyRuntime"
], function (FacadeCommandRuntime, FacadeCommandPolicyRuntime) {
    "use strict";

    return FacadeCommandPolicyRuntime.buildPolicy({
        execute: function (oController, sMethod, mInput) {
            return FacadeCommandRuntime.executeDetail(
                oController,
                oController && oController._facade,
                sMethod,
                mInput || {}
            );
        },
        methods: [
            "enterEdit",
            "open",
            "save",
            "close",
            "deleteChecklist",
            "discardChanges",
            "validate",
            "changeStatus",
            "rowOps",
            "resolveConflict",
            "attachmentLoad",
            "attachmentDelete",
            "attachmentUpload",
            "valueHelpLocation",
            "autosave",
            "personSuggest"
        ]
    });
});
