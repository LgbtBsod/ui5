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

        getCapabilities: function () {
            return Promise.resolve({
                contractVersion: "1.0.0",
                backendMode: "fake",
                features: {
                    lockStatus: true,
                    lockHeartbeat: true,
                    autoSave: true,
                    processAnalytics: true,
                    dictionaryLookup: true,
                    personSuggestion: true,
                    locationsHierarchy: true,
                    exportReport: true
                },
                compatibility: {
                    minUiContractVersion: "1.0.0",
                    maxUiContractVersion: "1.x"
                },
                source: "fake_backend"
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


        getChecklistRoot: function (sId) {
            return Promise.resolve(InMemoryDB.getCheckLists().find(function (o) { return o && o.root && o.root.id === sId; }) || null)
                .then(function (oItem) {
                    if (!oItem) { return null; }
                    return {
                        root: oItem.root || {},
                        basic: oItem.basic || {},
                        checks: [],
                        barriers: []
                    };
                });
        },

        getChecklistChecks: function (sId) {
            return Promise.resolve(InMemoryDB.getCheckLists().find(function (o) { return o && o.root && o.root.id === sId; }) || null)
                .then(function (oItem) { return (oItem && oItem.checks) || []; });
        },

        getChecklistBarriers: function (sId) {
            return Promise.resolve(InMemoryDB.getCheckLists().find(function (o) { return o && o.root && o.root.id === sId; }) || null)
                .then(function (oItem) { return (oItem && oItem.barriers) || []; });
        },

        createCheckList: function (oData) {
            return Promise.resolve(InMemoryDB.createCheckList(oData));
        },

        updateCheckList: function (sId, oData) {
            return Promise.resolve(InMemoryDB.updateCheckList(sId, oData));
        },

        lockAcquire: function () {
            return Promise.resolve({ success: true, action: "ACQUIRED", lock_expires: new Date(Date.now() + 5 * 60 * 1000).toISOString() });
        },

        lockHeartbeat: function () {
            return Promise.resolve({ success: true, is_killed: false, lock_expires: new Date(Date.now() + 5 * 60 * 1000).toISOString() });
        },

        lockStatus: function () {
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

        getFrontendConfig: function () {
            return Promise.resolve({
                search: { defaultMaxResults: 100, growingThreshold: 10 },
                timers: { heartbeatMs: 240000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 600000, autoSaveIntervalMs: 60000, autoSaveDebounceMs: 30000, networkGraceMs: 60000, cacheFreshMs: 30000, cacheStaleOkMs: 90000, analyticsRefreshMs: 900000 },
                source: "fallback_defaults",
                requiredFields: [
                    "/basic/date",
                    "/basic/time",
                    "/basic/timezone",
                    "/basic/OBSERVER_FULLNAME",
                    "/basic/OBSERVED_FULLNAME",
                    "/basic/LOCATION_KEY",
                    "/basic/LPC_KEY",
                    "/basic/PROF_KEY"
                ]
            });
        },


        getReferenceBundle: function () {
            return Promise.all([
                fetch("mock/persons.json").then(function (oResponse) { return oResponse.json(); }),
                fetch("mock/lpc.json").then(function (oResponse) { return oResponse.json(); }),
                fetch("mock/professions.json").then(function (oResponse) { return oResponse.json(); }),
                fetch("mock/location_hierarchy.json").then(function (oResponse) { return oResponse.json(); })
            ]).then(function (aData) {
                return {
                    persons: (aData[0] && aData[0].persons) || [],
                    dictionaries: {
                        LPC: (aData[1] && aData[1].lpc) || [],
                        PROFESSION: (aData[2] && aData[2].professions) || []
                    },
                    locations: (aData[3] && aData[3].locations) || [],
                    variables: { source: "fake_reference_bundle" }
                };
            });
        },


        getPersons: function () {
            return fetch("mock/persons.json")
                .then(function (oResponse) { return oResponse.json(); })
                .then(function (oData) { return (oData && oData.persons) || []; });
        },
        suggestPersons: function (sQuery) {
            var sNeedle = String(sQuery || "").trim().toLowerCase();
            return this.getPersons().then(function (aPersons) {
                return (aPersons || []).filter(function (oPerson) {
                    var sName = String((oPerson && oPerson.fullName) || "").toLowerCase();
                    var sPernr = String((oPerson && oPerson.perner) || "").toLowerCase();
                    var sPos = String((oPerson && oPerson.position) || "").toLowerCase();
                    return !sNeedle || sName.indexOf(sNeedle) >= 0 || sPernr.indexOf(sNeedle) >= 0 || sPos.indexOf(sNeedle) >= 0;
                }).slice(0, 10);
            });
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


        getProcessAnalytics: function () {
            var aRows = InMemoryDB.getCheckLists() || [];
            var iTotal = aRows.length;
            var iFailedChecks = 0;
            var iFailedBarriers = 0;
            var iClosed = 0;
            var iRegistered = 0;
            var nChecks = 0;
            var nBarriers = 0;

            aRows.forEach(function (oItem) {
                var oRoot = (oItem && oItem.root) || {};
                if (oRoot.has_failed_checks === true || oRoot.hasFailedChecks === true) {
                    iFailedChecks += 1;
                }
                if (oRoot.has_failed_barriers === true || oRoot.hasFailedBarriers === true) {
                    iFailedBarriers += 1;
                }
                if (String(oRoot.status || "").toUpperCase() === "CLOSED") {
                    iClosed += 1;
                }
                if (String(oRoot.status || "").toUpperCase() === "REGISTERED") {
                    iRegistered += 1;
                }
                nChecks += Number(oRoot.successRateChecks) || 0;
                nBarriers += Number(oRoot.successRateBarriers) || 0;
            });

            return Promise.resolve({
                total: iTotal,
                failedChecks: iFailedChecks,
                failedBarriers: iFailedBarriers,
                healthy: Math.max(0, iTotal - Math.max(iFailedChecks, iFailedBarriers)),
                closedCount: iClosed,
                registeredCount: iRegistered,
                avgChecksRate: iTotal ? Math.round(nChecks / iTotal) : 0,
                avgBarriersRate: iTotal ? Math.round(nBarriers / iTotal) : 0,
                refreshedAt: new Date().toISOString()
            });
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
