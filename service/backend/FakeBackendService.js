sap.ui.define([
    "sap_ui5/service/backend/InMemoryDB",
    "sap_ui5/service/backend/FakeODataService"
], function (InMemoryDB, FakeODataService) {
    "use strict";

    function _loadInitialChecklists() {
        return fetch("mock/checklists.json")
            .then(function (oResponse) {
                if (!oResponse.ok) {
                    throw new Error("Failed to load mock/checklists.json");
                }
                return oResponse.json();
            })
            .then(function (oData) {
                return oData && Array.isArray(oData.check_lists) ? oData.check_lists : [];
            });
    }

    return {

        login: function (username) {
            return Promise.resolve({
                sessionId: "sess-" + Date.now(),
                user: username
            });
        },

        init: function () {
            return _loadInitialChecklists().then(function (aChecklists) {
                InMemoryDB.init(aChecklists);
                return aChecklists;
            });
        },

        getCheckLists: function () {
            return Promise.resolve(InMemoryDB.getCheckLists());
        },

        queryCheckLists: function (mQuery) {
            return FakeODataService.readEntitySet("CheckLists", mQuery).then(function (oResult) {
                return oResult.results || [];
            });
        },

        createCheckList: function (oData) {
            return Promise.resolve(InMemoryDB.createCheckList(oData));
        },

        updateCheckList: function (sId, oData) {
            return Promise.resolve(InMemoryDB.updateCheckList(sId, oData));
        },

        deleteCheckList: function (sId) {
            return Promise.resolve(InMemoryDB.deleteCheckList(sId));
        },

        upsertRows: function (sId, sSection, aRows) {
            return Promise.resolve(InMemoryDB.upsertRows(sId, sSection, aRows));
        },

        lockAcquire: function () {
            return Promise.resolve({ success: true, action: "ACQUIRED", lock_expires: new Date(Date.now() + 5 * 60 * 1000).toISOString() });
        },

        lockHeartbeat: function () {
            return Promise.resolve({ success: true, is_killed: false, lock_expires: new Date(Date.now() + 5 * 60 * 1000).toISOString() });
        },

        lockRelease: function () {
            return Promise.resolve({ released: true, save_status: "N" });
        },

        buildReleaseBeaconPayload: function () {
            return null;
        },

        getServerState: function () {
            return FakeODataService.callFunctionImport("ServerState", {});
        },


        getPersons: function () {
            return fetch("mock/persons.json")
                .then(function (oResponse) { return oResponse.json(); })
                .then(function (oData) { return (oData && oData.persons) || []; });
        },

        getDictionary: function (sDomain) {
            var mMap = {
                LPC: "mock/lpc.json",
                PROFESSION: "mock/professions.json"
            };
            var sUrl = mMap[sDomain] || "mock/lpc.json";
            var sKey = sDomain === "PROFESSION" ? "professions" : "lpc";

            return fetch(sUrl)
                .then(function (oResponse) { return oResponse.json(); })
                .then(function (oData) { return (oData && oData[sKey]) || []; });
        },

        getLocations: function () {
            return fetch("mock/location_hierarchy.json")
                .then(function (oResponse) { return oResponse.json(); })
                .then(function (oData) { return (oData && oData.locations) || []; });
        },

        exportReport: function (sEntity, mPayload) {
            var mFilters = (mPayload && mPayload.filters) || {};
            var sSearchMode = (mPayload && mPayload.searchMode) || "EXACT";
            var sFilterId = String(mFilters.filterId || "").toLowerCase().trim();
            var sFilterLpc = mFilters.filterLpc || "";
            var sChecks = mFilters.filterFailedChecks || "ALL";
            var sBarriers = mFilters.filterFailedBarriers || "ALL";

            function evalBool(sFilter, bFailed) {
                if (sFilter === "ALL") { return true; }
                return (sFilter === "TRUE" && bFailed) || (sFilter === "FALSE" && !bFailed);
            }

            function pass(oItem) {
                var sId = String((((oItem || {}).root || {}).id) || "").toLowerCase();
                var sLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                var nChecks = Number((((oItem || {}).root || {}).successRateChecks));
                var nBarriers = Number((((oItem || {}).root || {}).successRateBarriers));
                var bChecks = Number.isFinite(nChecks) ? nChecks < 100 : false;
                var bBarriers = Number.isFinite(nBarriers) ? nBarriers < 100 : false;
                var bId = !sFilterId || sId.indexOf(sFilterId) >= 0;
                var bLpc = !sFilterLpc || sFilterLpc === sLpc;
                var bChecksOk = evalBool(sChecks, bChecks);
                var bBarriersOk = evalBool(sBarriers, bBarriers);

                if (sSearchMode === "LOOSE") {
                    var aRules = [
                        { enabled: !!sFilterId, value: bId },
                        { enabled: !!sFilterLpc, value: bLpc },
                        { enabled: sChecks !== "ALL", value: bChecksOk },
                        { enabled: sBarriers !== "ALL", value: bBarriersOk }
                    ].filter(function (oRule) { return oRule.enabled; });
                    return !aRules.length || aRules.some(function (oRule) { return oRule.value; });
                }

                return bId && bLpc && bChecksOk && bBarriersOk;
            }

            var aBase = InMemoryDB.getCheckLists().filter(pass);
            var aRows = [];

            aBase.forEach(function (oItem) {
                var oRoot = oItem.root || {};
                var oBasic = oItem.basic || {};
                var oBase = {
                    checklist_id: oRoot.id || "",
                    status: oRoot.status || "",
                    lpc: oBasic.LPC_KEY || "",
                    checks_percent: oRoot.successRateChecks,
                    barriers_percent: oRoot.successRateBarriers,
                    observer: oBasic.OBSERVER_FULLNAME || ""
                };

                if (sEntity === "barrier") {
                    (oItem.barriers || []).forEach(function (oBarrier, i) {
                        aRows.push(Object.assign({}, oBase, {
                            barrier_no: i + 1,
                            barrier_text: oBarrier.text || "",
                            barrier_result: !!oBarrier.result
                        }));
                    });
                } else if (sEntity === "check") {
                    (oItem.checks || []).forEach(function (oCheck, i) {
                        aRows.push(Object.assign({}, oBase, {
                            check_no: i + 1,
                            check_text: oCheck.text || "",
                            check_result: !!oCheck.result
                        }));
                    });
                } else {
                    aRows.push(oBase);
                }
            });

            return Promise.resolve({ rows: aRows });
        },


        create: function (payload) {
            return Promise.resolve(InMemoryDB.createObject(payload));
        },

        read: function (uuid) {
            return Promise.resolve(InMemoryDB.readObject(uuid));
        },

        update: function (uuid, payload) {
            return Promise.resolve(InMemoryDB.updateObject(uuid, payload));
        },

        getAll: function () {
            return Promise.resolve(InMemoryDB.getAll());
        }

    };
});
