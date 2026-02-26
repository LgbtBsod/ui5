sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService",
    "sap_ui5/service/backend/RealBackendService"
], function (FakeBackendService, RealBackendService) {
    "use strict";

    var _backendService = FakeBackendService;

    function _readUrlMode() {
        try {
            var oUrl = new URL(window.location.href);
            return oUrl.searchParams.get("backend");
        } catch (e) {
            return null;
        }
    }

    function _selectBackend(mConfig) {
        var sMode = (mConfig && mConfig.mode) || _readUrlMode() || "fake";
        _backendService = (sMode === "real") ? RealBackendService : FakeBackendService;

        if (_backendService.configure) {
            _backendService.configure(mConfig || {});
        }
    }

    return {

        configure: function (mConfig) {
            _selectBackend(mConfig || {});
        },

        getMode: function () {
            return _backendService === RealBackendService ? "real" : "fake";
        },


        login: function (username) {
            return _backendService.login(username);
        },

        init: function () {
            return _backendService.init();
        },

        getCheckLists: function () {
            return _backendService.getCheckLists();
        },

        queryCheckLists: function (mQuery) {
            return _backendService.queryCheckLists(mQuery);
        },


        getChecklistRoot: function (sId) {
            if (_backendService.getChecklistRoot) {
                return _backendService.getChecklistRoot(sId);
            }
            return _backendService.getCheckLists().then(function (aItems) {
                var oFound = (aItems || []).find(function (o) { return o && o.root && o.root.id === sId; });
                return oFound || null;
            });
        },

        getChecklistChecks: function (sId, mPaging) {
            if (_backendService.getChecklistChecks) {
                return _backendService.getChecklistChecks(sId, mPaging);
            }
            return _backendService.getCheckLists().then(function (aItems) {
                var oFound = (aItems || []).find(function (o) { return o && o.root && o.root.id === sId; });
                return (oFound && oFound.checks) || [];
            });
        },

        getChecklistBarriers: function (sId, mPaging) {
            if (_backendService.getChecklistBarriers) {
                return _backendService.getChecklistBarriers(sId, mPaging);
            }
            return _backendService.getCheckLists().then(function (aItems) {
                var oFound = (aItems || []).find(function (o) { return o && o.root && o.root.id === sId; });
                return (oFound && oFound.barriers) || [];
            });
        },

        createCheckList: function (oData) {
            return _backendService.createCheckList(oData);
        },

        updateCheckList: function (sId, oData, mOptions) {
            return _backendService.updateCheckList(sId, oData, mOptions || {});
        },

        autoSaveCheckList: function (sId, oDeltaPayload, oFullPayload, mOptions) {
            if (_backendService.autoSaveCheckList) {
                return _backendService.autoSaveCheckList(sId, oDeltaPayload || {}, oFullPayload || {}, mOptions || {});
            }
            return _backendService.updateCheckList(sId, oFullPayload || {}, mOptions || {});
        },

        deleteCheckList: function (sId) {
            return _backendService.deleteCheckList(sId);
        },

        upsertRows: function (sId, sSection, aRows) {
            return _backendService.upsertRows(sId, sSection, aRows);
        },

        lockAcquire: function (sObjectId, sSessionId, sUser, sStealFrom) {
            if (_backendService.lockAcquire) {
                return _backendService.lockAcquire(sObjectId, sSessionId, sUser, sStealFrom);
            }
            return Promise.resolve({ success: true, action: "ACQUIRED" });
        },

        lockHeartbeat: function (sObjectId, sSessionId) {
            return _backendService.lockHeartbeat(sObjectId, sSessionId);
        },

        lockStatus: function (sObjectId, sSessionId) {
            if (_backendService.lockStatus) {
                return _backendService.lockStatus(sObjectId, sSessionId);
            }
            return this.lockHeartbeat(sObjectId, sSessionId);
        },

        lockRelease: function (sObjectId, sSessionId, mOptions) {
            return _backendService.lockRelease(sObjectId, sSessionId, mOptions);
        },


        buildReleaseBeaconPayload: function (sObjectId, sSessionId, mOptions) {
            if (_backendService.buildReleaseBeaconPayload) {
                return _backendService.buildReleaseBeaconPayload(sObjectId, sSessionId, mOptions);
            }
            return null;
        },

        getServerState: function () {
            return _backendService.getServerState();
        },

        getFrontendConfig: function () {
            if (_backendService.getFrontendConfig) {
                return _backendService.getFrontendConfig();
            }
            return Promise.resolve({ search: { defaultMaxResults: 100, growingThreshold: 10 }, timers: { heartbeatMs: 240000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 600000, autoSaveIntervalMs: 60000, autoSaveDebounceMs: 30000, networkGraceMs: 60000, cacheFreshMs: 30000, cacheStaleOkMs: 90000, analyticsRefreshMs: 900000 }, source: "adapter_defaults" });
        },

        getPersons: function () {
            if (_backendService.getPersons) {
                return _backendService.getPersons();
            }
            return Promise.resolve([]);
        },

        getDictionary: function (sDomain) {
            if (_backendService.getDictionary) {
                return _backendService.getDictionary(sDomain);
            }
            return Promise.resolve([]);
        },

        suggestPersons: function (sQuery) {
            if (_backendService.suggestPersons) {
                return _backendService.suggestPersons(sQuery || "");
            }
            return this.getPersons().then(function (aItems) {
                var sNeedle = String(sQuery || "").trim().toLowerCase();
                return (aItems || []).filter(function (oPerson) {
                    var sName = String((oPerson && oPerson.fullName) || "").toLowerCase();
                    var sPernr = String((oPerson && oPerson.perner) || "").toLowerCase();
                    return !sNeedle || sName.indexOf(sNeedle) >= 0 || sPernr.indexOf(sNeedle) >= 0;
                }).slice(0, 10);
            });
        },

        getLocations: function () {
            if (_backendService.getLocations) {
                return _backendService.getLocations();
            }
            return Promise.resolve([]);
        },


        getProcessAnalytics: function (mPayload, sSearchMode) {
            if (_backendService.getProcessAnalytics) {
                return _backendService.getProcessAnalytics(mPayload || {}, sSearchMode || "EXACT");
            }
            return this.getCheckLists().then(function (aRows) {
                return {
                    total: (aRows || []).length,
                    monthly: (aRows || []).length,
                    failedChecks: 0,
                    failedBarriers: 0,
                    healthy: (aRows || []).length,
                    closedCount: 0,
                    registeredCount: 0,
                    avgChecksRate: 0,
                    avgBarriersRate: 0,
                    refreshedAt: new Date().toISOString()
                };
            });
        },

        getSimpleAnalytics: function () {
            if (_backendService.getSimpleAnalytics) {
                return _backendService.getSimpleAnalytics();
            }
            return this.getProcessAnalytics({}, "EXACT");
        },

        exportReport: function (sEntity, mPayload) {
            if (_backendService.exportReport) {
                return _backendService.exportReport(sEntity, mPayload || {});
            }
            return Promise.resolve({ rows: [] });
        },

        createObject: function (data) {
            return _backendService.create(data);
        },

        readObject: function (uuid) {
            return _backendService.read(uuid);
        },

        updateObject: function (uuid, data) {
            return _backendService.update(uuid, data);
        },

        getAllObjects: function () {
            return _backendService.getAll();
        }

    };
});
