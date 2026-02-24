sap.ui.define([
    "sap/base/Log"
], function (Log) {
    "use strict";

    function requestJson(sUrl, sCollectionName) {
        return fetch(sUrl)
            .then(function (oResponse) {
                if (!oResponse.ok) {
                    throw new Error("Request failed for " + sUrl + ": " + oResponse.status + " " + oResponse.statusText);
                }

                return oResponse.json();
            })
            .then(function (oData) {
                var vCollection = oData && oData[sCollectionName];

                if (!Array.isArray(vCollection)) {
                    throw new Error("Unexpected payload for " + sUrl + ": expected array in '" + sCollectionName + "'");
                }

                return vCollection;
            })
            .catch(function (oError) {
                Log.error("ChecklistService error", oError && oError.message, "sap_ui5.service.ChecklistService");
                throw oError;
            });
    }

    return {

        loadCheckLists: function () {
            return requestJson("mock/checklists.json", "check_lists");
        },

        loadPersons: function () {
            return requestJson("mock/persons.json", "persons");
        },

        loadLpc: function () {
            return requestJson("mock/lpc.json", "lpc");
        },

        loadProfessions: function () {
            return requestJson("mock/professions.json", "professions");
        },

        loadLocations: function () {
            return requestJson("mock/location_hierarchy.json", "locations");
        }

    };
});
