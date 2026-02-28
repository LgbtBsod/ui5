sap.ui.define([], function () {
    "use strict";

    var _baseUrl = "/sap/opu/odata/sap/Z_UI5_SRV";
    var _userId = "demoUser";
    var _sessionGuid = "sess-" + Date.now();
    var _csrfToken = "";
    var _referenceBundleCache = null;
    var _referenceBundleLoadedAt = 0;

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


    function _isLifecycleTracePath(sPath) {
        return /lock|autosave|checklist|update|create|release|heartbeat|status/i.test(String(sPath || ""));
    }

    function _logRequestPhase(sPhase, sPath, mDetails) {
        if (!_isLifecycleTracePath(sPath)) {
            return;
        }
        var fn = sPhase === "error" ? console.warn : console.info;
        fn.call(console, "[backend:" + sPhase + "]", Object.assign({ path: sPath }, mDetails || {}));
    }
    function _fetchCsrfToken() {
        return fetch(_joinUrl(_baseUrl, "/"), {
            method: "GET",
            credentials: "same-origin",
            headers: {
                "X-CSRF-Token": "Fetch"
            }
        }).then(function (oResponse) {
            if (!oResponse.ok) {
                throw new Error("Failed to fetch CSRF token");
            }
            _csrfToken = oResponse.headers.get("X-CSRF-Token") || "";
            return _csrfToken;
        });
    }

    function _request(sPath, mOptions) {
        var mOpts = mOptions || {};
        var sMethod = mOpts.method || "GET";
        var bWrite = ["POST", "PUT", "PATCH", "DELETE", "MERGE"].indexOf(String(sMethod).toUpperCase()) >= 0;
        var sUrl = _joinUrl(_baseUrl, sPath);

        if (mOpts.params) {
            var oParams = new URLSearchParams(mOpts.params);
            sUrl += (sUrl.indexOf("?") >= 0 ? "&" : "?") + oParams.toString();
        }

        _logRequestPhase("request", sPath, {
            method: sMethod,
            session_guid: _sessionGuid,
            hasBody: !!mOpts.body
        });

        function doFetch() {
            var mHeaders = Object.assign({ "Content-Type": "application/json" }, mOpts.headers || {});
            if (bWrite && _csrfToken) {
                mHeaders["X-CSRF-Token"] = _csrfToken;
            }
            return fetch(sUrl, {
            method: sMethod,
            credentials: "same-origin",
            headers: mHeaders,
            body: mOpts.body ? JSON.stringify(mOpts.body) : undefined
            });
        }

        function handleResponse(oResponse) {
            if (!oResponse.ok) {
                if (bWrite && oResponse.status === 403) {
                    return _fetchCsrfToken().then(function () {
                        return doFetch().then(handleResponse);
                    });
                }
                return oResponse.text().then(function (sText) {
                    _logRequestPhase("error", sPath, {
                        method: sMethod,
                        status: oResponse.status,
                        responseText: sText || ""
                    });
                    throw new Error(("HTTP " + oResponse.status + " for " + sPath + (sText ? ": " + sText : "")));
                });
            }

            if (oResponse.status === 204) {
                _logRequestPhase("response", sPath, { method: sMethod, status: oResponse.status });
                return null;
            }

            return oResponse.json().then(function (oJson) {
                _logRequestPhase("response", sPath, {
                    method: sMethod,
                    status: oResponse.status,
                    ok: true
                });
                return oJson;
            });
        }

        if (bWrite && !_csrfToken) {
            return _fetchCsrfToken().then(doFetch).then(handleResponse);
        }
        return doFetch().then(handleResponse);
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





    function _mapSearchRow(oRow) {
        return {
            root: {
                id: oRow.Key || oRow.db_key || oRow.id,
                db_key: oRow.Key || oRow.db_key || oRow.id,
                integrationFlag: true,
                this_is_integration_data: true,
                successRateChecks: 0,
                successRateBarriers: 0,
                status: _mapStatusToUi(oRow.Status || oRow.status)
            },
            basic: {
                date: oRow.DateCheck || oRow.date || "",
                time: "",
                timezone: "Europe/Amsterdam",
                equipment: oRow.EquipName || oRow.equipment || "",
                LPC_KEY: oRow.Lpc || oRow.lpc || "",
                checklist_id: oRow.Id || oRow.checklist_id || ""
            },
            checks: [],
            barriers: []
        };
    }

    function _acquireLock(sId, sStealFrom) {
        return _lockControl("ACQUIRE", sId, {
            sessionGuid: _sessionGuid,
            userId: _userId,
            stealFrom: sStealFrom || ""
        }).catch(function () {
            return _request("/lock/acquire", {
                method: "POST",
                params: {
                    object_uuid: sId,
                    session_guid: _sessionGuid,
                    uname: _userId,
                    iv_steal_from: sStealFrom || ""
                }
            });
        });
    }


    

    function _normalizeLockResponse(oRaw) {
        var o = oRaw || {};
        var bOk = (typeof o.Ok === "boolean") ? o.Ok : (typeof o.success === "boolean" ? o.success : true);
        var sReason = o.ReasonCode || o.reason_code || "";
        var bKilled = (typeof o.IsKilled === "boolean") ? o.IsKilled : !!o.is_killed;
        if (!bKilled && (sReason === "KILLED" || sReason === "LOCKED_BY_OTHER" || sReason === "EXPIRED")) {
            bKilled = true;
        }
        return {
            success: bOk,
            Ok: bOk,
            is_killed: bKilled,
            IsKilled: bKilled,
            reason_code: sReason,
            ReasonCode: sReason,
            lock_expires: o.ExpiresOn || o.lock_expires || null,
            lock_expires_on: o.ExpiresOn || o.lock_expires || null,
            owner: o.Owner || o.owner || ""
        };
    }

    function _lockControl(sAction, sObjectId, mExtra) {
        mExtra = mExtra || {};
        return _request("/LockControl", {
            method: "POST",
            body: {
                Action: sAction,
                RootKey: sObjectId,
                SessionGuid: mExtra.sessionGuid || _sessionGuid,
                Uname: mExtra.userId || _userId,
                StealFrom: mExtra.stealFrom || ""
            }
        }).then(function (oData) {
            return _normalizeLockResponse((oData && oData.d) || oData || { Ok: false });
        });
    }

    function _pickClientAgg(oPayload) {
        var v = (((oPayload || {}).meta || {}).aggChangedOn) || (((oPayload || {}).root || {}).changed_on) || (((oPayload || {}).root || {}).server_changed_on) || "";
        if (!v) { return null; }
        if (typeof v === "string" && /^\/Date\(\d+\)\/$/.test(v)) { return v; }
        var oDate = new Date(v);
        if (Number.isNaN(oDate.getTime())) { return null; }
        return "/Date(" + oDate.getTime() + ")/";
    }

    function _legacyToCanonicalChanges(oDeltaPayload, sRootId) {
        var aChanges = [];
        var oBasic = (oDeltaPayload && oDeltaPayload.basic) || {};
        if (Object.keys(oBasic).length) {
            aChanges.push({
                Entity: "BASIC",
                Key: sRootId,
                ParentKey: sRootId,
                EditMode: "U",
                Fields: {
                    LocationKey: oBasic.LOCATION_KEY,
                    LocationName: oBasic.LOCATION_NAME,
                    EquipName: oBasic.equipment || oBasic.EquipName || ""
                }
            });
        }
        ((oDeltaPayload && oDeltaPayload.checks) || []).forEach(function (oItem) {
            aChanges.push({
                Entity: "CHECK",
                Key: oItem.id || "",
                ParentKey: sRootId,
                EditMode: "U",
                Fields: {
                    ChecksNum: oItem.id || 0,
                    Comment: oItem.comment || "",
                    Result: !!oItem.result
                }
            });
        });
        ((oDeltaPayload && oDeltaPayload.barriers) || []).forEach(function (oItem) {
            aChanges.push({
                Entity: "BARRIER",
                Key: oItem.id || "",
                ParentKey: sRootId,
                EditMode: "U",
                Fields: {
                    BarriersNum: oItem.id || 0,
                    Comment: oItem.comment || "",
                    Result: !!oItem.result
                }
            });
        });
        return aChanges;
    }

    function _buildRequestGuid(sPrefix, sObjectId, oPayload) {
        var sRaw = sPrefix + "|" + String(sObjectId || "") + "|" + String(_sessionGuid || "") + "|" + JSON.stringify(oPayload || {});
        var iHash = 0;
        for (var i = 0; i < sRaw.length; i += 1) {
            iHash = ((iHash << 5) - iHash) + sRaw.charCodeAt(i);
            iHash |= 0;
        }
        return sPrefix + "-" + Math.abs(iHash);
    }

    function _getReferenceBundle(mOptions) {
        var iNow = Date.now();
        var bForce = !!(mOptions && mOptions.forceRefresh);
        if (!bForce && _referenceBundleCache && (iNow - _referenceBundleLoadedAt) < 5 * 60 * 1000) {
            return Promise.resolve(_referenceBundleCache);
        }
        return _request("/reference/bundle").then(function (oData) {
            var oBundle = (oData && oData.value) || {};
            _referenceBundleCache = oBundle;
            _referenceBundleLoadedAt = Date.now();
            return oBundle;
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

        getCapabilities: function () {
            return _request("/capabilities").catch(function () {
                return {
                    contractVersion: "1.0.0",
                    backendMode: "real",
                    features: {
                        lockStatus: true,
                        lockHeartbeat: true,
                        autoSave: true,
                        processAnalytics: true,
                        dictionaryLookup: true,
                        personSuggestion: true,
                        locationsHierarchy: true,
                        exportReport: true,
                        themePreferences: true
                    },
                    compatibility: {
                        minUiContractVersion: "1.0.0",
                        maxUiContractVersion: "1.x"
                    },
                    source: "real_backend_fallback"
                };
            });
        },

        getCheckLists: function () {
            return _request("/ChecklistSearchSet", {
                params: {
                    "$top": 200,
                    "$skip": 0,
                    "$inlinecount": "allpages"
                }
            }).then(function (oData) {
                var aRows = (oData && oData.d && oData.d.results) || [];
                return aRows.map(_mapSearchRow);
            }).catch(function () {
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
            });
        },

        queryCheckLists: function (mQuery) {
            var iTop = Number(mQuery && mQuery.maxResults);
            var sIdContains = String((mQuery && mQuery.idContains) || "").trim();
            var sLpcKey = String((mQuery && mQuery.lpcKey) || "").trim();
            var aFilterParts = [];

            if (!iTop || iTop < 1) {
                iTop = 9999;
            }
            iTop = Math.min(9999, iTop);

            if (sIdContains) {
                var sEscapedId = sIdContains.replace(/'/g, "''");
                aFilterParts.push("contains(Key,'" + sEscapedId + "') or contains(Id,'" + sEscapedId + "')");
            }
            if (sLpcKey) {
                var sEscapedLpc = sLpcKey.replace(/'/g, "''");
                aFilterParts.push("Lpc eq '" + sEscapedLpc + "'");
            }

            return _request("/ChecklistSearchSet", {
                params: {
                    "$top": iTop,
                    "$skip": 0,
                    "$filter": aFilterParts.length ? aFilterParts.join(" and ") : undefined
                }
            }).then(function (oData) {
                var aRows = (oData && oData.d && oData.d.results) || [];
                return aRows.map(_mapSearchRow);
            }).catch(function () {
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
            });
        },


        getLastChangeSet: function (sId) {
            var sKey = String(sId || "").replace(/-/g, "");
            return _request("/LastChangeSet('" + encodeURIComponent(sKey) + "')").then(function (oData) {
                var o = (oData && oData.d) || {};
                return {
                    RootKey: o.RootKey || sKey,
                    AggChangedOn: o.AggChangedOn || ""
                };
            });
        },

        getChecklistRoot: function (sId) {
            var sKey = String(sId || "").replace(/-/g, "");
            return Promise.all([
                _request("/ChecklistRootSet('" + encodeURIComponent(sKey) + "')"),
                _request("/ChecklistBasicInfoSet('" + encodeURIComponent(sKey) + "')"),
                _request("/LastChangeSet('" + encodeURIComponent(sKey) + "')"),
                _request("/ChecklistCheckSet", { params: { "$filter": "RootKey eq '" + sKey + "'", "$top": 20, "$skip": 0, "$inlinecount": "allpages" } }),
                _request("/ChecklistBarrierSet", { params: { "$filter": "RootKey eq '" + sKey + "'", "$top": 20, "$skip": 0, "$inlinecount": "allpages" } })
            ]).then(function (aData) {
                var oRoot = (aData[0] && aData[0].d) || {};
                var oBasic = (aData[1] && aData[1].d) || {};
                var oLastChange = (aData[2] && aData[2].d) || {};
                var aChecks = (((aData[3] || {}).d || {}).results || []).map(function (oCheck, iIndex) {
                    return {
                        id: oCheck.Key || (iIndex + 1),
                        text: oCheck.Comment || "",
                        comment: oCheck.Comment || "",
                        result: !!oCheck.Result
                    };
                });
                var aBarriers = (((aData[4] || {}).d || {}).results || []).map(function (oBarrier, iIndex) {
                    return {
                        id: oBarrier.Key || (iIndex + 1),
                        text: oBarrier.Comment || "",
                        comment: oBarrier.Comment || "",
                        result: !!oBarrier.Result
                    };
                });
                return {
                    root: {
                        id: oRoot.Key || sKey,
                        integrationFlag: true,
                        this_is_integration_data: true,
                        successRateChecks: Number(oRoot.SuccessChecksRate || 0),
                        successRateBarriers: Number(oRoot.SuccessBarriersRate || 0),
                        status: _mapStatusToUi(oRoot.Status || "")
                    },
                    basic: {
                        date: oBasic.DateCheck || "",
                        time: oBasic.TimeCheck || "",
                        timezone: oBasic.TimeZone || "UTC",
                        equipment: oBasic.EquipName || "",
                        LPC_KEY: oBasic.Lpc || "",
                        LPC_TEXT: oBasic.Lpc || "",
                        checklist_id: oRoot.Id || "",
                        OBSERVER_FULLNAME: oBasic.ObserverFullname || "",
                        OBSERVER_PERNER: oBasic.ObserverPernr || "",
                        OBSERVER_POSITION: oBasic.ObserverPosition || "",
                        OBSERVER_ORGUNIT: oBasic.ObserverOrgUnit || "",
                        OBSERVER_INTEGRATION_NAME: "",
                        OBSERVED_FULLNAME: oBasic.ObservedFullname || "",
                        OBSERVED_PERNER: oBasic.ObservedPernr || "",
                        OBSERVED_POSITION: oBasic.ObservedPosition || "",
                        OBSERVED_ORGUNIT: oBasic.ObservedOrgUnit || "",
                        OBSERVED_INTEGRATION_NAME: "",
                        LOCATION_KEY: oBasic.LocationKey || "",
                        LOCATION_NAME: oBasic.LocationName || "",
                        LOCATION_TEXT: oBasic.LocationName || ""
                    },
                    checks: aChecks,
                    barriers: aBarriers,
                    meta: {
                        aggChangedOn: oLastChange.AggChangedOn || oRoot.ChangedOn || ""
                    }
                };
            }).catch(function () {
                return _request("/checklist/" + encodeURIComponent(sId), { params: { "$expand": false } }).then(function (oRootLegacy) {
                    return {
                        root: {
                            id: oRootLegacy.id,
                            integrationFlag: (typeof oRootLegacy.this_is_integration_data === "boolean" ? oRootLegacy.this_is_integration_data : true),
                            this_is_integration_data: (typeof oRootLegacy.this_is_integration_data === "boolean" ? oRootLegacy.this_is_integration_data : true),
                            successRateChecks: 0,
                            successRateBarriers: 0,
                            status: _mapStatusToUi(oRootLegacy.status)
                        },
                        basic: {
                            date: oRootLegacy.date || "",
                            time: "",
                            timezone: "Europe/Amsterdam",
                            equipment: oRootLegacy.equipment || "",
                            LPC_KEY: oRootLegacy.lpc || "",
                            LPC_TEXT: oRootLegacy.lpc_text || "",
                            checklist_id: oRootLegacy.checklist_id || "",
                            OBSERVER_FULLNAME: oRootLegacy.observer_fullname || "",
                            OBSERVER_PERNER: oRootLegacy.observer_perner || "",
                            OBSERVER_POSITION: oRootLegacy.observer_position || "",
                            OBSERVER_ORGUNIT: oRootLegacy.observer_orgunit || "",
                            OBSERVER_INTEGRATION_NAME: oRootLegacy.observer_integration_name || "",
                            OBSERVED_FULLNAME: oRootLegacy.observed_fullname || "",
                            OBSERVED_PERNER: oRootLegacy.observed_perner || "",
                            OBSERVED_POSITION: oRootLegacy.observed_position || "",
                            OBSERVED_ORGUNIT: oRootLegacy.observed_orgunit || "",
                            OBSERVED_INTEGRATION_NAME: oRootLegacy.observed_integration_name || "",
                            LOCATION_KEY: oRootLegacy.location_key || "",
                            LOCATION_NAME: oRootLegacy.location_name || "",
                            LOCATION_TEXT: oRootLegacy.location_text || ""
                        },
                        checks: [],
                        barriers: []
                    };
                });
            });
        },

        getChecklistChecks: function (sId, mPaging) {
            var iTop = Number((mPaging && mPaging.top) || 20);
            var iSkip = Number((mPaging && mPaging.skip) || 0);
            var sKey = String(sId || "").replace(/-/g, "");
            return _request("/ChecklistCheckSet", {
                params: {
                    "$filter": "RootKey eq '" + sKey + "'",
                    "$top": iTop,
                    "$skip": iSkip,
                    "$inlinecount": "allpages"
                }
            }).then(function (oData) {
                return (((oData || {}).d || {}).results || []).map(function (oCheck, iIndex) {
                    return {
                        id: oCheck.Key || (iIndex + 1),
                        text: oCheck.Comment || "",
                        comment: oCheck.Comment || "",
                        result: !!oCheck.Result
                    };
                });
            }).catch(function () {
                return _request("/checklist/" + encodeURIComponent(sId) + "/checks", { params: { "$top": iTop, "$skip": iSkip } }).then(function (oDataLegacy) {
                    return ((oDataLegacy && oDataLegacy.value) || []).map(function (oCheckLegacy, iIndex) {
                        return {
                            id: oCheckLegacy.id || (iIndex + 1),
                            text: oCheckLegacy.text || "",
                            comment: "",
                            result: oCheckLegacy.status === "DONE"
                        };
                    });
                });
            });
        },

        getChecklistBarriers: function (sId, mPaging) {
            var iTop = Number((mPaging && mPaging.top) || 20);
            var iSkip = Number((mPaging && mPaging.skip) || 0);
            var sKey = String(sId || "").replace(/-/g, "");
            return _request("/ChecklistBarrierSet", {
                params: {
                    "$filter": "RootKey eq '" + sKey + "'",
                    "$top": iTop,
                    "$skip": iSkip,
                    "$inlinecount": "allpages"
                }
            }).then(function (oData) {
                return (((oData || {}).d || {}).results || []).map(function (oBarrier, iIndex) {
                    return {
                        id: oBarrier.Key || (iIndex + 1),
                        text: oBarrier.Comment || "",
                        comment: oBarrier.Comment || "",
                        result: !!oBarrier.Result
                    };
                });
            }).catch(function () {
                return _request("/checklist/" + encodeURIComponent(sId) + "/barriers", { params: { "$top": iTop, "$skip": iSkip } }).then(function (oDataLegacy) {
                    return ((oDataLegacy && oDataLegacy.value) || []).map(function (oBarrierLegacy, iIndex) {
                        return {
                            id: oBarrierLegacy.id || (iIndex + 1),
                            text: oBarrierLegacy.description || "",
                            comment: "",
                            result: !!oBarrierLegacy.is_active
                        };
                    });
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
            var aChecks = (oData && oData.checks) || [];
            var aBarriers = (oData && oData.barriers) || [];

            return _acquireLock(sId)
                .then(function () {
                    return _request("/SaveChanges", {
                        method: "POST",
                        body: {
                            RootKey: sId,
                            ClientAggChangedOn: _pickClientAgg(oData),
                            FullPayload: {
                                root: { Status: ((oData || {}).root || {}).status || "01" },
                                basic: {
                                    LocationKey: oBasic.LOCATION_KEY || "",
                                    LocationName: oBasic.LOCATION_NAME || "",
                                    EquipName: oBasic.equipment || ""
                                },
                                checks: aChecks.map(function (x, i) { return { ChecksNum: x.id || i + 1, Comment: x.comment || "", Result: !!x.result }; }),
                                barriers: aBarriers.map(function (x, i) { return { BarriersNum: x.id || i + 1, Comment: x.comment || "", Result: !!x.result }; })
                            }
                        }
                    }).catch(function () {
                        return _request("/actions/SaveChecklist", {
                            method: "POST",
                            params: {
                                root_id: sId,
                                user_id: _userId,
                                force: !!mOptions.force,
                                request_guid: _buildRequestGuid("SAVE", sId, oData)
                            },
                            body: {
                                lpc: oBasic.LPC_KEY || "L2",
                                basic: oBasic,
                                checks: aChecks,
                                barriers: aBarriers
                            }
                        });
                    });
                })
                .then(function () {
                    var oRoot = ((oData || {}).root || {});
                    return _mapChecklist({
                        id: sId,
                        checklist_id: ((oBasic && oBasic.checklist_id) || ""),
                        lpc: oBasic.LPC_KEY || "",
                        status: oRoot.status || "01",
                        date: oBasic.date || "",
                        equipment: oBasic.equipment || ""
                    }, { checks: aChecks, barriers: aBarriers });
                });
        },

        autoSaveCheckList: function (sId, oDeltaPayload, oFullPayload, mOptions) {
            mOptions = mOptions || {};
            var oBasic = (oDeltaPayload && oDeltaPayload.basic) || {};

            return _acquireLock(sId)
                .then(function () {
                    return _request("/AutoSave", {
                        method: "POST",
                        body: {
                            RootKey: sId,
                            ClientAggChangedOn: _pickClientAgg(oDeltaPayload) || _pickClientAgg(oFullPayload),
                            Changes: _legacyToCanonicalChanges(oDeltaPayload, sId)
                        }
                    }).catch(function () {
                        return _request("/actions/AutoSaveChecklist", {
                            method: "POST",
                            params: {
                                root_id: sId,
                                user_id: _userId,
                                force: !!mOptions.force,
                                request_guid: _buildRequestGuid("AUTOSAVE", sId, oDeltaPayload)
                            },
                            body: {
                                lpc: oBasic.LPC_KEY,
                                basic: oBasic,
                                checks: (oDeltaPayload && oDeltaPayload.checks) || [],
                                barriers: (oDeltaPayload && oDeltaPayload.barriers) || []
                            }
                        });
                    });
                })
                .then(function () {
                    return _mapChecklist({
                        id: sId,
                        checklist_id: ((oBasic && oBasic.checklist_id) || ""),
                        lpc: oBasic.LPC_KEY || "",
                        status: (((oFullPayload || {}).root || {}).status || "01"),
                        date: oBasic.date || "",
                        equipment: oBasic.equipment || ""
                    }, {
                        checks: (oFullPayload && oFullPayload.checks) || [],
                        barriers: (oFullPayload && oFullPayload.barriers) || []
                    });
                });
        },

        setChecklistStatus: function (sId, sNewStatus, oPayload) {
            return _request("/SetChecklistStatus", {
                method: "POST",
                body: {
                    RootKey: sId,
                    NewStatus: sNewStatus,
                    ClientAggChangedOn: _pickClientAgg(oPayload)
                }
            }).then(function (oData) {
                return (oData && oData.d) || oData || {};
            }).catch(function () {
                return _request("/actions/SetChecklistStatus", {
                    method: "POST",
                    body: {
                        RootKey: sId,
                        NewStatus: sNewStatus,
                        ClientAggChangedOn: _pickClientAgg(oPayload)
                    }
                }).then(function (oData) {
                    return (oData && oData.d) || oData || {};
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
            return _lockControl("ACQUIRE", sObjectId, {
                sessionGuid: _sessionGuid,
                userId: _userId,
                stealFrom: sStealFrom || ""
            }).catch(function () {
                return _request("/actions/LockAcquire", {
                    method: "POST",
                    params: {
                        root_id: sObjectId,
                        user_id: _userId,
                        session_guid: _sessionGuid,
                        iv_steal_from: sStealFrom || ""
                    }
                }).then(function (oData) {
                    return _normalizeLockResponse((oData && oData.d) || oData || { success: false });
                });
            });
        },

        lockHeartbeat: function (sObjectId, sSessionId) {
            if (!sObjectId) {
                return Promise.resolve({ Ok: true, IsKilled: false });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            return _lockControl("HEARTBEAT", sObjectId, {
                sessionGuid: _sessionGuid
            }).catch(function () {
                return _request("/actions/LockHeartbeat", {
                    method: "POST",
                    params: {
                        root_id: sObjectId,
                        session_guid: _sessionGuid
                    }
                }).then(function (oData) {
                    return _normalizeLockResponse((oData && oData.d) || oData || { success: false });
                });
            });
        },

        lockStatus: function (sObjectId, sSessionId) {
            if (!sObjectId) {
                return Promise.resolve({ Ok: true, ReasonCode: "FREE", IsKilled: false });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            return _request("/LockStatusSet('" + encodeURIComponent(sObjectId) + "')", {
                params: {
                    SessionGuid: _sessionGuid,
                    Uname: _userId
                }
            }).then(function (oData) {
                return _normalizeLockResponse((oData && oData.d) || { Ok: true, ReasonCode: "FREE", IsKilled: false });
            }).catch(function () {
                return _request("/actions/LockStatus", {
                    method: "POST",
                    params: {
                        root_id: sObjectId,
                        session_guid: _sessionGuid
                    }
                }).then(function (oData) {
                    return _normalizeLockResponse((oData && oData.d) || oData || { success: false });
                });
            });
        },

        lockRelease: function (sObjectId, sSessionId, mOptions) {
            if (!sObjectId) {
                return Promise.resolve({ Ok: true, ReasonCode: "FREE", IsKilled: false });
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            mOptions = mOptions || {};
            return _lockControl("RELEASE", sObjectId, {
                sessionGuid: _sessionGuid
            }).catch(function () {
                return _request("/actions/LockRelease", {
                    method: "POST",
                    params: {
                        root_id: sObjectId,
                        session_guid: _sessionGuid,
                        iv_try_save: !!mOptions.trySave
                    },
                    body: mOptions.payload || {}
                }).then(function (oData) {
                    return _normalizeLockResponse((oData && oData.d) || oData || { released: false });
                });
            });
        },

        buildReleaseBeaconPayload: function (sObjectId, sSessionId, mOptions) {
            if (!sObjectId) {
                return null;
            }
            if (sSessionId) { _sessionGuid = sSessionId; }
            mOptions = mOptions || {};
            return {
                url: _joinUrl(_baseUrl, "/LockControl"),
                body: {
                    Action: "RELEASE",
                    RootKey: sObjectId,
                    SessionGuid: _sessionGuid,
                    Uname: _userId,
                    StealFrom: "",
                    TrySave: !!mOptions.trySave,
                    Payload: mOptions.payload || {}
                }
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
                    timers: { heartbeatMs: 240000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 600000, autoSaveIntervalMs: 60000, autoSaveDebounceMs: 30000, networkGraceMs: 60000, cacheFreshMs: 30000, cacheStaleOkMs: 90000, analyticsRefreshMs: 600000, cacheToleranceMs: 15000 },
                source: "fallback_defaults",
                    variables: { validationSource: "real_frontend_fallback" },
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
            return _getReferenceBundle().then(function (oBundle) {
                return oBundle.persons || [];
            }).catch(function () {
                return _request("/persons/suggest", { params: { search: "" } }).then(function (oData) {
                    return oData && oData.value ? oData.value : [];
                });
            });
        },

        suggestPersons: function (sQuery) {
            return _request("/persons/suggest", { params: { search: sQuery || "" } }).then(function (oData) {
                return oData && oData.value ? oData.value : [];
            });
        },

        getDictionary: function (sDomain) {
            return _request("/DictionaryItemSet", {
                params: {
                    "$filter": "Domain eq '" + String(sDomain || "").replace(/'/g, "''") + "'"
                }
            }).then(function (oData) {
                var aRows = (oData && oData.d && oData.d.results) || [];
                return aRows.map(function (oRow) {
                    return { key: oRow.Key, text: oRow.Text, domain: oRow.Domain };
                });
            }).catch(function () {
                return _getReferenceBundle().then(function (oBundle) {
                    var mDict = oBundle.dictionaries || {};
                    return mDict[sDomain] || [];
                }).catch(function () {
                    return _request("/dict", { params: { domain: sDomain } }).then(function (oData) {
                        return oData && oData.value ? oData.value : [];
                    });
                });
            });
        },

        getLocations: function () {
            var sToday = new Date().toISOString().slice(0, 10);
            return _request("/GetHierarchy", { params: { DateCheck: sToday, Method: "MPL" } }).then(function (oData) {
                return (oData && oData.d && oData.d.results) || [];
            }).catch(function () {
                return _request("/actions/GetMplHierarchy", { method: "POST", params: { date: sToday } }).then(function (oData) {
                    return (oData && oData.d && oData.d.results) || [];
                });
            }).catch(function () {
                return _getReferenceBundle().then(function (oBundle) {
                    return oBundle.locations || [];
                });
            }).catch(function () {
                return _request("/location", { params: { date: sToday } }).catch(function () {
                    return _request("/hierarchy", { params: { date: sToday } });
                }).then(function (oData) {
                    return oData && oData.value ? oData.value : [];
                });
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
            return _request("/ReportExport", {
                method: "POST",
                body: {
                    RootKeys: (mPayload && mPayload.keys) || []
                }
            }).catch(function () {
                return _request("/actions/export", {
                    method: "POST",
                    body: {
                        entity: sEntity || "checklist",
                        filters: (mPayload && mPayload.filters) || {},
                        search_mode: (mPayload && mPayload.searchMode) || "EXACT"
                    }
                });
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
