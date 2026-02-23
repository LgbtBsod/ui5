sap.ui.define([
    "sap/ui/model/json/JSONModel"
], function (JSONModel) {
    "use strict";

    return {

        createDataModel: function () {
            return new JSONModel({
                checkLists: [],
                selectedChecklist: null
            });
        },

        createStateModel: function () {
            return new JSONModel({
                mode: "READ",
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
                locations: []
            });
        }

    };
});