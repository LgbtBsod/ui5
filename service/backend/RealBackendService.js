sap.ui.define([], function () {
    "use strict";

    var _baseUrl = "http://localhost:8000";
    var _userId = "demoUser";

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



    function _acquireLock(sId) {
        return _request("/lock/acquire", {
            method: "POST",
            params: {
                pcct_uuid: sId,
                user_id: _userId
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
            return Promise.resolve({
                sessionId: _userId,
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

        updateCheckList: function (sId, oData) {
            var sLpc = (oData && oData.basic && oData.basic.LPC_KEY) || "L2";
            var aChecks = (oData && oData.checks) || [];
            var aBarriers = (oData && oData.barriers) || [];

            return _acquireLock(sId)
                .then(function () {
                    return _request("/checklist/" + encodeURIComponent(sId) + "/autosave", {
                        method: "PATCH",
                        params: {
                            user_id: _userId,
                            force: true
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

        lockHeartbeat: function () {
            return Promise.resolve({ status: "OK" });
        },

        lockRelease: function () {
            return Promise.resolve({ status: "RELEASED" });
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
