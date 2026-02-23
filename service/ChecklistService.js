sap.ui.define([
    "sap/base/Log"
], function (Log) {
    "use strict";

    return {

        loadCheckLists: function () {
            return fetch("mock/checklists.json")
                .then(res => res.json())
                .then(data => data.check_lists);
        },

        loadPersons: function () {
            return fetch("mock/persons.json")
                .then(res => res.json())
                .then(data => data.persons);
        },

        loadLpc: function () {
            return fetch("mock/lpc.json")
                .then(res => res.json())
                .then(data => data.lpc);
        },

        loadProfessions: function () {
            return fetch("mock/professions.json")
                .then(res => res.json())
                .then(data => data.professions);
        },

        loadLocations: function () {
            return fetch("mock/location_hierarchy.json")
                .then(res => res.json())
                .then(data => data.locations);
        }

    };
});