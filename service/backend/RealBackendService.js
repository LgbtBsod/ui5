sap.ui.define([], function () {
    "use strict";

    var _baseUrl = "http://localhost:8000";
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
        return {
            root: {
                id: oRoot.id,
                integrationFlag: true,
                successRateChecks: 0,
                successRateBarriers: 0,
                status: _mapStatusToUi(oRoot.status)
            },
            basic: {
                date: "",
                time: "",
                timezone: "Europe/Amsterdam",
                LPC_KEY: oRoot.lpc || "",
                checklist_id: oRoot.checklist_id || ""
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
                    top: 200,
                    skip: 0
                }
            }).then(function (oList) {
                var aRoots = oList && oList.value ? oList.value : [];

                return Promise.all(aRoots.map(function (oRoot) {
                    return _request("/checklist/" + encodeURIComponent(oRoot.id), {
                        params: { expand: true }
                    }).then(function (oDetails) {
                        return _mapChecklist(oRoot, oDetails);
                    }).catch(function () {
                        return _mapChecklist(oRoot, {});
                    });
                }));
            });
        },

        queryCheckLists: function () {
            return this.getCheckLists();
        },


        getChecklistRoot: function (sId) {
            return _request("/checklist/" + encodeURIComponent(sId), { params: { expand: false } }).then(function (oRoot) {
                return {
                    root: {
                        id: oRoot.id,
                        integrationFlag: true,
                        successRateChecks: 0,
                        successRateBarriers: 0,
                        status: _mapStatusToUi(oRoot.status)
                    },
                    basic: {
                        date: "",
                        time: "",
                        timezone: "Europe/Amsterdam",
                        LPC_KEY: oRoot.lpc || "",
                        checklist_id: oRoot.checklist_id || ""
                    },
                    checks: [],
                    barriers: []
                };
            });
        },

        getChecklistChecks: function (sId) {
            return _request("/checklist/" + encodeURIComponent(sId), { params: { expand: true } }).then(function (oDetails) {
                return ((oDetails && oDetails.checks) || []).map(function (oCheck, iIndex) {
                    return {
                        id: oCheck.id || (iIndex + 1),
                        text: oCheck.text || "",
                        comment: "",
                        result: oCheck.status === "DONE"
                    };
                });
            });
        },

        getChecklistBarriers: function (sId) {
            return _request("/checklist/" + encodeURIComponent(sId), { params: { expand: true } }).then(function (oDetails) {
                return ((oDetails && oDetails.barriers) || []).map(function (oBarrier, iIndex) {
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
            mOptions = mOptions || {};
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
                        }).catch(function () { return null; });
                    })
                    .then(function () {
                        return _request("/checklist/" + encodeURIComponent(sNewId), { params: { expand: true } });
                    })
                    .then(function (oDetails) {
                        return _mapChecklist(oCreated || {
                            id: sNewId,
                            checklist_id: sChecklistId,
                            lpc: sLpc,
                            status: "01"
                        }, oDetails);
                    });
            });
        },

        updateCheckList: function (sId, oData, mOptions) {
            mOptions = mOptions || {};
            var sLpc = (oData && oData.basic && oData.basic.LPC_KEY) || "L2";
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
                            lpc: sLpc
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
                    return _request("/checklist/" + encodeURIComponent(sId), { params: { expand: true } });
                })
                .then(function (oDetails) {
                    return _mapChecklist({
                        id: sId,
                        checklist_id: ((oData && oData.basic && oData.basic.checklist_id) || sId),
                        lpc: sLpc,
                        status: "01"
                    }, oDetails);
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

        getPersons: function () {
            return _request("/persons/suggest", { params: { search: "" } }).then(function (oData) {
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
            return _request("/location", { params: { date: sToday } }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
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
