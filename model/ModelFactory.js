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
                searchMode: "EXACT",
                // Search cap requested by user; empty means load all available rows.
                searchMaxResults: "100",
                isBusy: false,
                isDirty: false,
                isLocked: false,
                isKilled: false,
                hasConflict: false,
                sessionId: null,
                activeObjectId: null,
                lockExpires: null,
                idleExpires: null,
                requiresUserLogin: true,
                testUser: "",
                testUserLogin: "",
                lockOperationPending: false,
                lockOperationState: "IDLE",
                lockOperationText: "",
                navGuardBypass: false,
                networkOnline: true,
                networkGraceMode: false,
                networkGraceExpiresAt: null,
                autosaveState: "IDLE",
                autosaveAt: null,
                requiredFields: []
            });
        },

        createLayoutModel: function () {
            return new JSONModel({
                smartFilter: {
                    useCustomSmartFilters: true,
                    variant: "default",
                    fields: []
                },
                smartTable: {
                    useCustomSmartTable: true,
                    variant: "default",
                    columns: [],
                    selectionMode: "SingleSelectMaster"
                },
                personalization: {
                    compactRows: false,
                    showHints: true
                }
            });
        },

        createCacheModel: function () {
            return new JSONModel({
                pristineSnapshot: null,
                keyMapping: {},
                lastServerState: null
            });
        },

        createMasterDataModel: function () {
            return new JSONModel({
                persons: [],
                lpc: [],
                professions: [],
                statuses: ["SUCCESS", "WARNING", "CRITICAL"],
                resultTypes: ["PASS", "FAIL"],
                types: [],
                timezones: [
                    { key: "Europe/Amsterdam", text: "Europe/Amsterdam" },
                    { key: "UTC", text: "UTC" }
                ]
            });
        },

        createMplModel: function () {
            return new JSONModel({
                locations: []
            });
        }

    };
});
