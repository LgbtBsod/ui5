sap.ui.define([
    "sap_ui5/service/backend/FakeBackendService",
    "sap_ui5/service/backend/RealBackendService"
], function (FakeBackendService, RealBackendService) {
    "use strict";

    var _backendService = FakeBackendService;
    var _CAPABILITY_CONTRACT_VERSION = "1.0.0";
    var _UI_CONTRACT_VERSION = "1.0.0";

    function _defaultCapabilities(sMode) {
        return {
            contractVersion: _CAPABILITY_CONTRACT_VERSION,
            backendMode: sMode || "fake",
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
            source: "adapter_defaults"
        };
    }

    function _normalizeCapabilities(oRaw, sMode) {
        var oFallback = _defaultCapabilities(sMode);
        var oFeatures = (oRaw && oRaw.features) ? Object.assign({}, oRaw.features) : Object.assign({}, oFallback.features);
        var oCompatibility = Object.assign({}, oFallback.compatibility, (oRaw && oRaw.compatibility) || {});
        return {
            contractVersion: String((oRaw && oRaw.contractVersion) || oFallback.contractVersion),
            backendMode: String((oRaw && oRaw.backendMode) || sMode || oFallback.backendMode),
            features: oFeatures,
            compatibility: oCompatibility,
            source: String((oRaw && oRaw.source) || oFallback.source)
        };
    }


    function _parseSemver(sVersion) {
        var m = String(sVersion || "").trim().match(/^(\d+)\.(\d+)\.(\d+)(?:[-+].*)?$/);
        if (!m) {
            return null;
        }
        return {
            major: Number(m[1]),
            minor: Number(m[2]),
            patch: Number(m[3])
        };
    }

    function _compareSemver(a, b) {
        if (a.major !== b.major) {
            return a.major - b.major;
        }
        if (a.minor !== b.minor) {
            return a.minor - b.minor;
        }
        return a.patch - b.patch;
    }

    function _parseMajorRange(sRangeMax) {
        var m = String(sRangeMax || "").trim().match(/^(\d+)\.x$/);
        if (!m) {
            return null;
        }
        var iMajor = Number(m[1]);
        return {
            lower: { major: iMajor, minor: 0, patch: 0 },
            upperExclusive: { major: iMajor + 1, minor: 0, patch: 0 }
        };
    }

    function _buildMaxCompatibilityBound(sMaxUiContractVersion) {
        var oExact = _parseSemver(sMaxUiContractVersion);
        if (oExact) {
            return {
                kind: "exact",
                upperInclusive: oExact
            };
        }

        var oMajorRange = _parseMajorRange(sMaxUiContractVersion);
        if (oMajorRange) {
            return {
                kind: "major_range",
                lower: oMajorRange.lower,
                upperExclusive: oMajorRange.upperExclusive
            };
        }
        return null;
    }

    function _validateSemverPolicy(oCapabilities) {
        var oCompatibility = (oCapabilities && oCapabilities.compatibility) || {};
        var oMin = _parseSemver(oCompatibility.minUiContractVersion);
        var oMaxBound = _buildMaxCompatibilityBound(oCompatibility.maxUiContractVersion);

        if (!oMin || !oMaxBound) {
            return {
                ok: false,
                reason: "invalid_semver_metadata"
            };
        }

        if (oMaxBound.kind === "exact" && _compareSemver(oMin, oMaxBound.upperInclusive) > 0) {
            return {
                ok: false,
                reason: "invalid_semver_policy_range"
            };
        }

        if (oMaxBound.kind === "major_range") {
            if (_compareSemver(oMin, oMaxBound.upperExclusive) >= 0 || _compareSemver(oMin, oMaxBound.lower) < 0) {
                return {
                    ok: false,
                    reason: "invalid_semver_policy_range"
                };
            }
        }

        return {
            ok: true,
            reason: "valid_semver_policy",
            minVersion: oMin,
            maxBound: oMaxBound
        };
    }

    function _isUiContractVersionCompatible(sUiVersion, oCapabilities) {
        var oUiVersion = _parseSemver(sUiVersion);
        var oSemverPolicy = _validateSemverPolicy(oCapabilities);

        if (!oUiVersion || !oSemverPolicy.ok) {
            return {
                ok: false,
                reason: oSemverPolicy.ok ? "invalid_semver_metadata" : oSemverPolicy.reason
            };
        }

        var oMin = oSemverPolicy.minVersion;
        var oMaxBound = oSemverPolicy.maxBound;

        if (_compareSemver(oUiVersion, oMin) < 0) {
            return {
                ok: false,
                reason: "ui_contract_below_min"
            };
        }

        if (oMaxBound.kind === "exact") {
            if (_compareSemver(oUiVersion, oMaxBound.upperInclusive) > 0) {
                return {
                    ok: false,
                    reason: "ui_contract_out_of_supported_range"
                };
            }
        } else if (_compareSemver(oUiVersion, oMaxBound.lower) < 0 || _compareSemver(oUiVersion, oMaxBound.upperExclusive) >= 0) {
            return {
                ok: false,
                reason: "ui_contract_out_of_supported_range"
            };
        }

        return {
            ok: true,
            reason: "compatible"
        };
    }

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
            _UI_CONTRACT_VERSION = String((mConfig && mConfig.uiContractVersion) || _UI_CONTRACT_VERSION || _CAPABILITY_CONTRACT_VERSION);
            _selectBackend(mConfig || {});
        },

        getMode: function () {
            return _backendService === RealBackendService ? "real" : "fake";
        },

        getCapabilities: function () {
            var sMode = this.getMode();
            if (_backendService.getCapabilities) {
                return Promise.resolve(_backendService.getCapabilities()).then(function (oCapabilities) {
                    return _normalizeCapabilities(oCapabilities, sMode);
                });
            }
            return Promise.resolve(_defaultCapabilities(sMode));
        },

        negotiateCapabilities: function (aRequiredFeatures) {
            var aRequired = Array.isArray(aRequiredFeatures) ? aRequiredFeatures : [];
            return this.getCapabilities().then(function (oCapabilities) {
                var mFeatures = (oCapabilities && oCapabilities.features) || {};
                var aMissing = aRequired.filter(function (sFeature) {
                    return !mFeatures[sFeature];
                });
                return {
                    ok: aMissing.length === 0,
                    missingFeatures: aMissing,
                    capabilities: oCapabilities
                };
            });
        },


        ensureContractCompatibility: function (sUiContractVersion) {
            return this.getCapabilities().then(function (oCapabilities) {
                var oCompatibility = _isUiContractVersionCompatible(sUiContractVersion, oCapabilities);
                return {
                    ok: oCompatibility.ok,
                    reason: oCompatibility.reason,
                    uiContractVersion: String(sUiContractVersion || ""),
                    capabilities: oCapabilities
                };
            });
        },

        enforceContractCompatibility: function (sUiContractVersion) {
            return this.ensureContractCompatibility(sUiContractVersion).then(function (oResult) {
                if (!oResult.ok) {
                    throw new Error("Backend capability contract incompatible: " + oResult.reason + " (ui=" + oResult.uiContractVersion + ")");
                }
                return oResult;
            });
        },


        login: function (username) {
            return _backendService.login(username);
        },

        init: function () {
            var sUiContractVersion = String(_UI_CONTRACT_VERSION || _CAPABILITY_CONTRACT_VERSION);
            return this.enforceContractCompatibility(sUiContractVersion).then(function () {
                return _backendService.init();
            });
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

        getLastChangeSet: function (sId) {
            if (_backendService.getLastChangeSet) {
                return _backendService.getLastChangeSet(sId);
            }
            return Promise.resolve({ RootKey: sId, AggChangedOn: "" });
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

        setChecklistStatus: function (sId, sNewStatus, oPayload) {
            if (_backendService.setChecklistStatus) {
                return _backendService.setChecklistStatus(sId, sNewStatus, oPayload || {});
            }
            return Promise.resolve({ RootKey: sId, Status: sNewStatus });
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

        executeBatch: function (aOperations) {
            if (_backendService.executeBatch) {
                return _backendService.executeBatch(aOperations || []);
            }
            return Promise.resolve({ __batchResponses: [] });
        },

        getFrontendConfig: function () {
            if (_backendService.getFrontendConfig) {
                return _backendService.getFrontendConfig();
            }
            return Promise.resolve({ search: { defaultMaxResults: 100, growingThreshold: 10 }, timers: { heartbeatMs: 240000, lockStatusMs: 60000, gcdMs: 300000, idleMs: 600000, autoSaveIntervalMs: 60000, autoSaveDebounceMs: 30000, networkGraceMs: 60000, cacheFreshMs: 30000, cacheStaleOkMs: 90000, analyticsRefreshMs: 900000 }, source: "adapter_defaults", variables: { validationSource: "adapter_defaults" } });
        },


        getReferenceBundle: function (mOptions) {
            if (_backendService.getReferenceBundle) {
                return _backendService.getReferenceBundle(mOptions || {});
            }
            return Promise.resolve({ persons: [], dictionaries: {}, locations: [], variables: {} });
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
