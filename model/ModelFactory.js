sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function (JSONModel) {
    "use strict";

    return {

        createDataModel: function () {
            return new JSONModel({
                checkLists: [],
                visibleCheckLists: [],
                selectedChecklist: null
            });
        },

        createStateModel: function () {
            return new JSONModel({
                mode: "READ",
                layout: "OneColumn",
                isLoading: false,
                loadError: false,
                loadErrorMessage: "",
                filterId: "",
                filterLpc: "",
                filterFailedChecks: "ALL",
                filterFailedBarriers: "ALL",
                isBusy: false,
                isDirty: false,
                isLocked: false,
                hasConflict: false,
                sessionId: null,
                lockExpires: null,
                idleExpires: null
            });
        },

        createReferenceModel: function () {
            return new JSONModel({
                persons: [],
                lpc: [],
                professions: [],
                locations: [],
                timezones: [
                    { key: "Europe/Amsterdam", text: "Europe/Amsterdam" },
                    { key: "UTC", text: "UTC" }
                ]
            });
        }

    };
});
