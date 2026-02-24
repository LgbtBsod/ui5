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

            return _request("/checklist/", {
                method: "POST",
                params: {
                    checklist_id: sChecklistId,
                    lpc: sLpc,
                    user_id: _userId
                }
            }).then(function (oCreated) {
                return _mapChecklist(oCreated || {
                    id: sChecklistId,
                    checklist_id: sChecklistId,
                    lpc: sLpc,
                    status: "01"
                }, {});
            });
        },

        updateCheckList: function (sId, oData) {
            var sLpc = (oData && oData.basic && oData.basic.LPC_KEY) || "L2";

            return _request("/checklist/" + encodeURIComponent(sId) + "/autosave", {
                method: "PATCH",
                params: {
                    user_id: _userId,
                    force: true
                },
                body: {
                    lpc: sLpc
                }
            }).then(function () {
                return oData;
            });
        },

        deleteCheckList: function () {
            return Promise.reject(new Error("DELETE is not implemented on mock_gateway backend API"));
        },

        upsertRows: function () {
            return Promise.resolve({ status: "SKIPPED" });
        },

        lockHeartbeat: function (sSessionId) {
            return _request("/lock/heartbeat", {
                method: "POST",
                params: {
                    pcct_uuid: sSessionId,
                    user_id: _userId
                }
            });
        },

        lockRelease: function (sSessionId) {
            return _request("/lock/release", {
                method: "POST",
                params: {
                    pcct_uuid: sSessionId,
                    user_id: _userId
                }
            });
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
