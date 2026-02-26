sap.ui.define([], function () {
    "use strict";

    var _baseUrl = "http://localhost:5000";   // ← ВАЖНО: не const
    var _userId = "demoUser";
    var _sessionGuid = "sess-" + Date.now();

    function _joinUrl(sBase, sPath) {
        var bBaseEnds = /\/$/.test(sBase);
        var bPathStarts = /^\//.test(sPath);

        if (bBaseEnds && bPathStarts) {
            return sBase + sPath.slice(1);
        }
        if (!bBaseEnds && !bPathStarts) {
            return sBase + "/" + sPath;
        }
        return sBase + sPath;
    }

    function _request(sPath, mOptions) {
        var mOpts = mOptions || {};
        var sUrl = _joinUrl(_baseUrl, sPath);

        if (mOpts.params) {
            var oParams = new URLSearchParams(mOpts.params);
            sUrl += (sUrl.indexOf("?") >= 0 ? "&" : "?") + oParams.toString();
        }

        return fetch(sUrl, {
            method: mOpts.method || "GET",
            headers: Object.assign({ "Content-Type": "application/json" }, mOpts.headers || {}),
            body: mOpts.body ? JSON.stringify(mOpts.body) : undefined
        }).then(function (oResponse) {
            if (!oResponse.ok) {
                return oResponse.text().then(function (sText) {
                    throw new Error(("HTTP " + oResponse.status + " for " + sPath + (sText ? ": " + sText : "")));
                });
            }

            if (oResponse.status === 204) {
                return null;
            }

            return oResponse.json();
        });
    }

    function _mapStatusToUi(sStatus) {
        if (sStatus === "SUBMITTED") {
            return "SUCCESS";
        }
        return "WARNING";
    }

    function _mapChecklist(oRoot, oDetails) {
        var oData = oDetails || {};
        var bIntegrationData = typeof oRoot.this_is_integration_data === "boolean" ? oRoot.this_is_integration_data : true;
        return {
            root: {
                id: oRoot.id,
                integrationFlag: bIntegrationData,
                this_is_integration_data: bIntegrationData,
                successRateChecks: 0,
                successRateBarriers: 0,
                status: _mapStatusToUi(oRoot.status)
            },
            basic: {
                date: oRoot.date || "",
                time: "",
                timezone: "Europe/Amsterdam",
                equipment: oRoot.equipment || "",
                LPC_KEY: oRoot.lpc || "",
                LPC_TEXT: oRoot.lpc_text || "",
                checklist_id: oRoot.checklist_id || "",
                OBSERVER_FULLNAME: oRoot.observer_fullname || "",
                OBSERVER_PERNER: oRoot.observer_perner || "",
                OBSERVER_POSITION: oRoot.observer_position || "",
                OBSERVER_ORGUNIT: oRoot.observer_orgunit || "",
                OBSERVER_INTEGRATION_NAME: oRoot.observer_integration_name || "",
                OBSERVED_FULLNAME: oRoot.observed_fullname || "",
                OBSERVED_PERNER: oRoot.observed_perner || "",
                OBSERVED_POSITION: oRoot.observed_position || "",
                OBSERVED_ORGUNIT: oRoot.observed_orgunit || "",
                OBSERVED_INTEGRATION_NAME: oRoot.observed_integration_name || "",
                LOCATION_KEY: oRoot.location_key || "",
                LOCATION_NAME: oRoot.location_name || "",
                LOCATION_TEXT: oRoot.location_text || ""
            },
            checks: (oData.checks || []).map(function (oCheck, iIndex) {
                return {
                    id: oCheck.id || (iIndex + 1),
                    text: oCheck.text || "",
                    comment: "",
                    result: oCheck.status === "DONE"
                };
            }),
            barriers: (oData.barriers || []).map(function (oBarrier, iIndex) {
                return {
                    id: oBarrier.id || (iIndex + 1),
                    text: oBarrier.description || "",
                    comment: "",
                    result: !!oBarrier.is_active
                };
            })
        };
    }



    function _acquireLock(sId, sStealFrom) {
        return _request("/lock/acquire", {
            method: "POST",
            params: {
                object_uuid: sId,
                session_guid: _sessionGuid,
                uname: _userId,
                iv_steal_from: sStealFrom || ""
            }
        });
    }
    return {
        configure: function (mConfig) {
            mConfig = mConfig || {};
            _baseUrl = mConfig.baseUrl || _baseUrl;
        },

        login: function (username) {
            _userId = username || _userId;
            _sessionGuid = "sess-" + Date.now() + "-" + Math.floor(Math.random() * 10000);
            return Promise.resolve({
                sessionId: _sessionGuid,
                user: _userId
            });
        },

        init: function () {
            return Promise.resolve([]);
        },

        getCheckLists: function () {
            return _request("/checklist", {
                params: {
                    "$top": 200,
                    "$skip": 0
                }
            }).then(function (oList) {
                var aRoots = oList && oList.value ? oList.value : [];
                return aRoots.map(function (oRoot) {
                    return _mapChecklist(oRoot, { checks: [], barriers: [] });
                });
            });
        },

        queryCheckLists: function (mQuery) {
            var iTop = Number(mQuery && mQuery.maxResults);
            var sIdContains = String((mQuery && mQuery.idContains) || "").trim();
            var sLpcKey = String((mQuery && mQuery.lpcKey) || "").trim();
            var aFilterParts = [];

            // Empty max means "all" for enterprise search use-case.
            if (!iTop || iTop < 1) {
                iTop = 9999;
            }
            iTop = Math.min(9999, iTop);

            if (sIdContains) {
                var sEscapedId = sIdContains.replace(/'/g, "''");
                aFilterParts.push("contains(id,'" + sEscapedId + "') or contains(checklist_id,'" + sEscapedId + "')");
            }
            if (sLpcKey) {
                var sEscapedLpc = sLpcKey.replace(/'/g, "''");
                aFilterParts.push("lpc eq '" + sEscapedLpc + "'");
            }

            return _request("/checklist", {
                params: {
                    "$top": iTop,
                    "$skip": 0,
                    "$filter": aFilterParts.length ? aFilterParts.join(" and ") : undefined
                }
            }).then(function (oList) {
                var aRoots = oList && oList.value ? oList.value : [];
                return aRoots.map(function (oRoot) {
                    return _mapChecklist(oRoot, { checks: [], barriers: [] });
                });
            });
        },


        getChecklistRoot: function (sId) {
            return _request("/checklist/" + encodeURIComponent(sId), { params: { "$expand": false } }).then(function (oRoot) {
                return {
                    root: {
                        id: oRoot.id,
                        integrationFlag: (typeof oRoot.this_is_integration_data === "boolean" ? oRoot.this_is_integration_data : true),
                        this_is_integration_data: (typeof oRoot.this_is_integration_data === "boolean" ? oRoot.this_is_integration_data : true),
                        successRateChecks: 0,
                        successRateBarriers: 0,
                        status: _mapStatusToUi(oRoot.status)
                    },
                    basic: {
                        date: oRoot.date || "",
                        time: "",
                        timezone: "Europe/Amsterdam",
                        equipment: oRoot.equipment || "",
                        LPC_KEY: oRoot.lpc || "",
                        LPC_TEXT: oRoot.lpc_text || "",
                        checklist_id: oRoot.checklist_id || "",
                        OBSERVER_FULLNAME: oRoot.observer_fullname || "",
                        OBSERVER_PERNER: oRoot.observer_perner || "",
                        OBSERVER_POSITION: oRoot.observer_position || "",
                        OBSERVER_ORGUNIT: oRoot.observer_orgunit || "",
                        OBSERVER_INTEGRATION_NAME: oRoot.observer_integration_name || "",
                        OBSERVED_FULLNAME: oRoot.observed_fullname || "",
                        OBSERVED_PERNER: oRoot.observed_perner || "",
                        OBSERVED_POSITION: oRoot.observed_position || "",
                        OBSERVED_ORGUNIT: oRoot.observed_orgunit || "",
                        OBSERVED_INTEGRATION_NAME: oRoot.observed_integration_name || "",
                        LOCATION_KEY: oRoot.location_key || "",
                        LOCATION_NAME: oRoot.location_name || "",
                        LOCATION_TEXT: oRoot.location_text || ""
                    },
                    checks: [],
                    barriers: []
                };
            });
        },

        getChecklistChecks: function (sId, mPaging) {
            var iTop = Number((mPaging && mPaging.top) || 20);
            var iSkip = Number((mPaging && mPaging.skip) || 0);
            return _request("/checklist/" + encodeURIComponent(sId) + "/checks", { params: { "$top": iTop, "$skip": iSkip } }).then(function (oData) {
                return ((oData && oData.value) || []).map(function (oCheck, iIndex) {
                    return {
                        id: oCheck.id || (iIndex + 1),
                        text: oCheck.text || "",
                        comment: "",
                        result: oCheck.status === "DONE"
                    };
                });
            });
        },

        getChecklistBarriers: function (sId, mPaging) {
            var iTop = Number((mPaging && mPaging.top) || 20);
            var iSkip = Number((mPaging && mPaging.skip) || 0);
            return _request("/checklist/" + encodeURIComponent(sId) + "/barriers", { params: { "$top": iTop, "$skip": iSkip } }).then(function (oData) {
                return ((oData && oData.value) || []).map(function (oBarrier, iIndex) {
                    return {
                        id: oBarrier.id || (iIndex + 1),
                        text: oBarrier.description || "",
                        comment: "",
                        result: !!oBarrier.is_active
                    };
                });
            });
        },

createCheckList: function (oData) {

    var sChecklistId = (oData && oData.root && oData.root.id) || ("CL-" + Date.now());
    var sLpc = (oData && oData.basic && oData.basic.LPC_KEY) || "L2";
    var aChecks = (oData && oData.checks) || [];
    var aBarriers = (oData && oData.barriers) || [];

    return _request("/checklist/", {
        method: "POST",
        params: {
            checklist_id: sChecklistId,
            lpc: sLpc,
            user_id: _userId
        }
    }).then(function (oCreated) {

        var sNewId = (oCreated && oCreated.id) || sChecklistId;

        return _acquireLock(sNewId)
            .then(function () {
                return _request("/checklist/" + encodeURIComponent(sNewId) + "/checks", {
                    method: "PUT",
                    params: { user_id: _userId },
                    body: { rows: aChecks }
                });
            })
            .then(function () {
                return _request("/checklist/" + encodeURIComponent(sNewId) + "/barriers", {
                    method: "PUT",
                    params: { user_id: _userId },
                    body: { rows: aBarriers }
                }).catch(function () {
                    return null;
                });
            })
            .then(function () {
                return _request("/checklist/" + encodeURIComponent(sNewId), {
                    params: { expand: false }
                });
            })
            .then(function (oRoot) {
                return _mapChecklist(oRoot || oCreated || {
                    id: sNewId,
                    checklist_id: sChecklistId,
                    lpc: sLpc,
                    status: "01"
                }, { checks: aChecks, barriers: aBarriers });
            });
    });
},
        updateCheckList: function (sId, oData, mOptions) {
            mOptions = mOptions || {};
            var oBasic = (oData && oData.basic) || {};
            var sLpc = oBasic.LPC_KEY || "L2";
            var aChecks = (oData && oData.checks) || [];
            var aBarriers = (oData && oData.barriers) || [];

            return _acquireLock(sId)
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId) + "/autosave", {
                        method: "PATCH",
                        params: {
                            user_id: _userId,
                            force: !!mOptions.force
                        },
                        body: {
                            lpc: sLpc,
                            basic: oBasic
                        }
                    });
                })
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId) + "/checks", {
                        method: "PUT",
                        params: { user_id: _userId },
                        body: { rows: aChecks }
                    });
                })
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId) + "/barriers", {
                        method: "PUT",
                        params: { user_id: _userId },
                        body: { rows: aBarriers }
                    }).catch(function () { return null; });
                })
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId), { params: { "$expand": false } });
                })
                .then(function (oDetails) {
                    return _mapChecklist({
                        id: sId,
                        checklist_id: ((oData && oData.basic && oData.basic.checklist_id) || sId),
                        lpc: sLpc,
                        status: "01"
                    }, { checks: aChecks, barriers: aBarriers });
                });
        },

        autoSaveCheckList: function (sId, oDeltaPayload, oFullPayload, mOptions) {
            mOptions = mOptions || {};
            var oBasic = (oDeltaPayload && oDeltaPayload.basic) || {};
            var bHasRows = !!((oDeltaPayload && oDeltaPayload.checks && oDeltaPayload.checks.length) || (oDeltaPayload && oDeltaPayload.barriers && oDeltaPayload.barriers.length));

            if (bHasRows) {
                // Current mock gateway row API is replace-style; for safety fallback to full update.
                return this.updateCheckList(sId, oFullPayload || {}, mOptions);
            }

            return _acquireLock(sId)
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId) + "/autosave", {
                        method: "PATCH",
                        params: {
                            user_id: _userId,
                            force: !!mOptions.force
                        },
                        body: {
                            lpc: oBasic.LPC_KEY,
                            basic: oBasic
                        }
                    });
                })
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId), { params: { "$expand": false } });
                })
                .then(function (oRoot) {
                    return _mapChecklist(oRoot || { id: sId }, {
                        checks: (oFullPayload && oFullPayload.checks) || [],
                        barriers: (oFullPayload && oFullPayload.barriers) || []
                    });
                });
        },

        deleteCheckList: function (sId) {
            return _acquireLock(sId).then(function () {
                return _request("/checklist/" + encodeURIComponent(sId), {
                    method: "DELETE",
                    params: { user_id: _userId }
                });
            });
        },

        upsertRows: function (sId, sSection, aRows) {
            return _acquireLock(sId).then(function () {
                return _request("/checklist/" + encodeURIComponent(sId) + "/" + encodeURIComponent(sSection), {
                    method: "PUT",
                    params: { user_id: _userId },
                    body: { rows: aRows || [] }
                });
            });
        },

        lockAcquire: function (sObjectId, sSessionId, sUser, sStealFrom) {
            if (sSessionId) { _sessionGuid = sSessionId; }
            if (sUser) { _userId = sUser; }
            return _acquireLock(sObjectId, sStealFrom);
        },

        lockHeartbeat: function (sObjectId, sSessionId) {
            if (!sObjectId) {
                return Promise.resolve({ success: true, is_killed: false });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            return _request("/lock/heartbeat", {
                method: "POST",
                params: {
                    object_uuid: sObjectId,
                    session_guid: _sessionGuid
                }
            });
        },

        lockStatus: function (sObjectId, sSessionId) {
            if (!sObjectId) {
                return Promise.resolve({ success: true, is_killed: false });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            return _request("/lock/status", {
                method: "POST",
                params: {
                    object_uuid: sObjectId,
                    session_guid: _sessionGuid
                }
            });
        },

        lockRelease: function (sObjectId, sSessionId, mOptions) {
            if (!sObjectId) {
                return Promise.resolve({ released: true, save_status: "N" });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            mOptions = mOptions || {};
            return _request("/lock/release", {
                method: "POST",
                params: {
                    object_uuid: sObjectId,
                    session_guid: _sessionGuid,
                    iv_try_save: !!mOptions.trySave
                },
                body: mOptions.payload || {}
            });
        },

        buildReleaseBeaconPayload: function (sObjectId, sSessionId, mOptions) {
            if (!sObjectId) {
                return null;
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            mOptions = mOptions || {};
            var oParams = new URLSearchParams({
                object_uuid: sObjectId,
                session_guid: _sessionGuid,
                iv_try_save: !!mOptions.trySave
            });
            return {
                url: _joinUrl(_baseUrl, "/lock/release") + "?" + oParams.toString(),
                body: mOptions.payload || {}
            };
        },

        getServerState: function () {
            return Promise.resolve({
                backend: "mock_gateway",
                checkedAt: new Date().toISOString()
            });
        },

        getFrontendConfig: function () {
            return _request("/config/frontend").catch(function () {
                return {
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
                };
            });
        },

        getPersons: function () {
            return _request("/persons/suggest", { params: { search: "" } }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
            });
        },

        suggestPersons: function (sQuery) {
            return _request("/persons/suggest", { params: { search: sQuery || "" } }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
            });
        },

        getDictionary: function (sDomain) {
            return _request("/dict", { params: { domain: sDomain } }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
            });
        },

        getLocations: function () {
            var sToday = new Date().toISOString().slice(0, 10);
            return _request("/location", { params: { date: sToday } }).catch(function () {
                return _request("/hierarchy", { params: { date: sToday } });
            }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
            });
        },

        getProcessAnalytics: function (mPayload, sSearchMode) {
            var mParams = {
                id: (mPayload && mPayload.filterId) || "",
                lpc: (mPayload && mPayload.filterLpc) || "",
                failed_checks: (mPayload && mPayload.filterFailedChecks) || "ALL",
                failed_barriers: (mPayload && mPayload.filterFailedBarriers) || "ALL",
                search_mode: sSearchMode || "EXACT"
            };

            Object.keys(mParams).forEach(function (sKey) {
                var vValue = mParams[sKey];
                if (typeof vValue === "string" && !vValue.trim()) {
                    delete mParams[sKey];
                }
            });

            return _request("/WorkflowAnalytics", { params: mParams }).catch(function () {
                return _request("/analytics/process", { params: mParams });
            });
        },

        getSimpleAnalytics: function () {
            return _request("/SimpleAnalytical").catch(function () {
                return _request("/WorkflowAnalytics");
            }).catch(function () {
                return _request("/analytics/process");
            });
        },

        exportReport: function (sEntity, mPayload) {
            return _request("/actions/export", {
                method: "POST",
                body: {
                    entity: sEntity || "checklist",
                    filters: (mPayload && mPayload.filters) || {},
                    search_mode: (mPayload && mPayload.searchMode) || "EXACT"
                }
            });
        },


        create: function (payload) {
            return Promise.resolve(payload);
        },

        read: function () {
            return Promise.resolve(null);
        },

        update: function (_, payload) {
            return Promise.resolve(payload);
        },

        getAll: function () {
            return this.getCheckLists();
        }
    };
});
