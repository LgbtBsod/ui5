sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter",
    "sap_ui5/manager/SmartCacheManager",
    "sap_ui5/manager/HeartbeatManager",
    "sap_ui5/manager/GCDManager",
    "sap_ui5/manager/ActivityMonitor",
    "sap_ui5/manager/BeaconManager",
    "sap_ui5/manager/AutoSaveCoordinator",
    "sap_ui5/manager/ConnectivityCoordinator",
    "sap_ui5/manager/LockStatusMonitor",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap_ui5/util/FlowCoordinator",
    "sap_ui5/util/DeltaPayloadBuilder",
    "sap/ui/model/odata/v2/ODataModel",
    "sap_ui5/service/usecase/ComponentStartupDiagnosticsOrchestrationUseCase"
], function (
    UIComponent,
    ModelFactory,
    BackendAdapter,
    SmartSearchAdapter,
    SmartCacheManager,
    HeartbeatManager,
    GCDManager,
    ActivityMonitor,
    BeaconManager,
    AutoSaveCoordinator,
    ConnectivityCoordinator,
    LockStatusMonitor,
    JSONModel,
    MessageBox,
    FlowCoordinator,
    DeltaPayloadBuilder,
    ODataModel,
    ComponentStartupDiagnosticsOrchestrationUseCase
) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            UIComponent.prototype.init.apply(this, arguments);

            var sConfiguredMode = this.getManifestEntry("/sap.ui5/config/backendMode") || "fake";
            var bLocalHost = (window && window.location && /localhost|127\.0\.0\.1/.test(window.location.hostname || ""));
            var sUiContractVersion = this.getManifestEntry("/sap.ui5/config/uiContractVersion") || "1.0.0";
            BackendAdapter.configure({
                mode: (sConfiguredMode === "real" && bLocalHost) ? "fake" : sConfiguredMode,
                uiContractVersion: sUiContractVersion,
                baseUrl: this.getManifestEntry("/sap.app/dataSources/mainService/uri") || "http://localhost:5000"
            });

            var oDataModel = ModelFactory.createDataModel();
            var oStateModel = ModelFactory.createStateModel();
            var fnSyncCapabilityDiagnostics = ComponentStartupDiagnosticsOrchestrationUseCase.createCapabilitySync({
                stateModel: oStateModel,
                getBackendMode: BackendAdapter.getMode
            });
            var oMainServiceModel = new ODataModel({
                serviceUrl: this.getManifestEntry("/sap.app/dataSources/mainService/uri") || "http://localhost:5000/",
                useBatch: true,
                defaultCountMode: "Inline",
                refreshAfterChange: false
            });
            oMainServiceModel.setDeferredGroups(["changes", "autosave", "saveFlow"]);
            oMainServiceModel.setChangeGroups({
                "*": {
                    groupId: "changes",
                    changeSetId: "ChecklistSave",
                    single: false
                }
            });
            ComponentStartupDiagnosticsOrchestrationUseCase.wireMetadataEvents({
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                syncCapability: fnSyncCapabilityDiagnostics
            });

            var oLayoutModel = ModelFactory.createLayoutModel();
            var oCacheModel = ModelFactory.createCacheModel();
            var oMasterDataModel = ModelFactory.createMasterDataModel();
            var oMplModel = ModelFactory.createMplModel();
            var oEnvModel = ModelFactory.createEnvModel();
            var mTimerDefaults = oStateModel.getProperty("/timers") || {};
            var fnFormatHumanDateTime = function (vDate) {
                var oDate = vDate instanceof Date ? vDate : new Date(vDate || Date.now());
                if (Number.isNaN(oDate.getTime())) {
                    oDate = new Date();
                }
                return oDate.toLocaleString(undefined, {
                    year: "numeric",
                    month: "short",
                    day: "2-digit",
                    hour: "2-digit",
                    minute: "2-digit"
                });
            };
            var fnEventPayload = function (oEvent) {
                return (oEvent && typeof oEvent.getParameters === "function" && oEvent.getParameters()) || {};
            };
            var fnApplyLockProbeState = function (oPayload) {
                var bKilled = !!(oPayload && oPayload.is_killed);
                oStateModel.setProperty("/isKilled", bKilled);
                oStateModel.setProperty("/lockExpires", (oPayload && oPayload.lock_expires) || null);
                return bKilled;
            };

            this._oSmartCache = new SmartCacheManager({ freshMs: mTimerDefaults.cacheFreshMs, staleOkMs: mTimerDefaults.cacheStaleOkMs });
            this._oHeartbeat = new HeartbeatManager({
                intervalMs: Number(mTimerDefaults.heartbeatMs) || 4 * 60 * 1000,
                heartbeatFn: function () {
                    // Do not send heartbeat outside explicit edit/lock mode.
                    if (oStateModel.getProperty("/mode") !== "EDIT" || !oStateModel.getProperty("/isLocked")) {
                        return Promise.resolve({ success: true, is_killed: false, skipped: true });
                    }
                    return BackendAdapter.lockHeartbeat(
                        oStateModel.getProperty("/activeObjectId"),
                        oStateModel.getProperty("/sessionId")
                    );
                }
            });
            this._oGcd = new GCDManager({ intervalMs: Number(mTimerDefaults.gcdMs) || 5 * 60 * 1000 });
            this._oActivity = new ActivityMonitor({ idleMs: Number(mTimerDefaults.idleMs) || 10 * 60 * 1000 });
            this._oAutoSave = new AutoSaveCoordinator({
                intervalMs: Number(mTimerDefaults.autoSaveIntervalMs) || 60 * 1000,
                debounceMs: Number(mTimerDefaults.autoSaveDebounceMs) || 30 * 1000,
                shouldSave: function () {
                    return oStateModel.getProperty("/mode") === "EDIT"
                        && !!oStateModel.getProperty("/isDirty")
                        && !!oStateModel.getProperty("/activeObjectId")
                        && oStateModel.getProperty("/networkOnline") !== false;
                },
                buildPayload: function () {
                    var sId = oStateModel.getProperty("/activeObjectId");
                    var oObj = oDataModel.getProperty("/object");
                    var oSelected = this.getModel("selected").getData() || {};
                    var oCurrent = (oObj && oObj.root && oObj.root.id === sId) ? oObj : oSelected;
                    if (!sId || !oCurrent || !oCurrent.root) {
                        return null;
                    }

                    var oBase = (oDataModel.getProperty("/objectOriginal/root/id") === sId)
                        ? (oDataModel.getProperty("/objectOriginal") || {})
                        : (oDataModel.getProperty("/selectedChecklist") || {});

                    var oDelta = DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oBase);
                    if (!oDelta) {
                        return null;
                    }

                    return { id: sId, payload: oDelta, fullPayload: JSON.parse(JSON.stringify(oCurrent)) };
                }.bind(this),
                saveFn: function (oPayload) {
                    return BackendAdapter.autoSaveCheckList(oPayload.id, oPayload.payload, oPayload.fullPayload, { force: false }).then(function (oSaved) {
                        oStateModel.setProperty("/isDirty", false);
                        if (oSaved && oSaved.root && oSaved.root.id === oPayload.id) {
                            if (oDataModel.getProperty("/object/root/id") === oPayload.id) {
                                oDataModel.setProperty("/objectOriginal", JSON.parse(JSON.stringify(oSaved)));
                            }
                        }
                        return oSaved;
                    });
                }
            });
            this._oAutoSave.attachEvent("autosaveStart", function () {
                oStateModel.setProperty("/autosaveState", "SAVING");
                console.info("[autosave] start", { objectId: oStateModel.getProperty("/activeObjectId") || null });
            });
            this._oAutoSave.attachEvent("autosaveDone", function () {
                oStateModel.setProperty("/autosaveState", "SAVED");
                oStateModel.setProperty("/autosaveAt", new Date().toISOString());
                console.info("[autosave] done", { objectId: oStateModel.getProperty("/activeObjectId") || null });
            });
            this._oAutoSave.attachEvent("autosaveError", function (oEvent) {
                oStateModel.setProperty("/autosaveState", "ERROR");
                console.warn("[autosave] error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            });

            this._oConnectivity = new ConnectivityCoordinator({ graceMs: Number(mTimerDefaults.networkGraceMs) || 60 * 1000 });
            this._oLockStatus = new LockStatusMonitor({
                intervalMs: Number(mTimerDefaults.lockStatusMs) || 60 * 1000,
                checkFn: function () {
                    // Fast killed-lock probe (1 minute) that complements heartbeat.
                    if (oStateModel.getProperty("/mode") !== "EDIT" || !oStateModel.getProperty("/isLocked")) {
                        return Promise.resolve({ success: true, is_killed: false, skipped: true });
                    }
                    return BackendAdapter.lockStatus(
                        oStateModel.getProperty("/activeObjectId"),
                        oStateModel.getProperty("/sessionId")
                    );
                }
            });
            // Centralized transition used by both heartbeat and lock-status probe.
            this._handleKilledLock = function (oPayload) {
                oStateModel.setProperty("/isKilled", true);
                oStateModel.setProperty("/lockExpires", (oPayload && oPayload.lock_expires) || null);
                oStateModel.setProperty("/mode", "READ");
                oStateModel.setProperty("/isLocked", false);
                this._oHeartbeat.stop();
                this._oLockStatus.stop();
                this._oGcd.destroyManager();
                MessageBox.warning(this.getModel("i18n").getResourceBundle().getText("lockKilledMessage"));
                FlowCoordinator.releaseWithTrySave({ getModel: this.getModel.bind(this), getResourceBundle: function(){ return this.getModel("i18n").getResourceBundle();}.bind(this) });
            }.bind(this);

            this._fnUnregisterBeacon = BeaconManager.register(function () {
                return BackendAdapter.buildReleaseBeaconPayload(
                    oStateModel.getProperty("/activeObjectId"),
                    oStateModel.getProperty("/sessionId"),
                    { trySave: true }
                );
            });

            this._oHeartbeat.attachEvent("heartbeat", function (oEvent) {
                var oPayload = fnEventPayload(oEvent);
                console.info("[lock] heartbeat", oPayload);
                if (fnApplyLockProbeState(oPayload)) {
                    this._handleKilledLock(oPayload);
                }
                var sCheckedAt = fnFormatHumanDateTime(new Date());
                oCacheModel.setProperty("/lastServerState", {
                    lastChangeSet: oPayload.last_change_set || null,
                    serverChangedOn: oPayload.server_changed_on || null,
                    checkedAt: sCheckedAt
                });
                oStateModel.setProperty("/cacheValidationAt", sCheckedAt);
            }.bind(this));


            this._oHeartbeat.attachEvent("heartbeatError", function (oEvent) {
                oStateModel.setProperty("/hasConflict", true);
                console.warn("[lock] heartbeat error", fnEventPayload(oEvent));
            });

            this._oGcd.attachEvent("gcdExpired", function () {
                oStateModel.setProperty("/hasConflict", true);
            });

            this._oLockStatus.attachEvent("status", function (oEvent) {
                var oPayload = fnEventPayload(oEvent);
                if (fnApplyLockProbeState(oPayload)) {
                    this._handleKilledLock(oPayload);
                    return;
                }
                oStateModel.setProperty("/hasConflict", false);
            }.bind(this));

            this._oLockStatus.attachEvent("statusError", function () {
                // status probe is best-effort; heartbeat remains the source of truth.
                oStateModel.setProperty("/hasConflict", true);
            });

            this._oActivity.attachEvent("idleTimeout", function () {
                oStateModel.setProperty("/idleExpires", new Date().toISOString());
                oStateModel.setProperty("/mode", "READ");
            });


            // Full save resets GCD by architecture rule; autosave should not.
            this._fnOnFullSave = function () {
                this._oGcd.resetOnFullSave();
            }.bind(this);
            window.addEventListener("pcct:fullSave", this._fnOnFullSave);

            this.setModel(new JSONModel({}), "selected");
            this.setModel(oDataModel, "data");
            this.setModel(oStateModel, "state");
            this.setModel(oMainServiceModel, "mainService");
            this.setModel(oLayoutModel, "layout");
            this.setModel(oCacheModel, "cache");
            this.setModel(oMasterDataModel, "masterData");
            this.setModel(oMplModel, "mpl");
            this.setModel(oEnvModel, "env");


            var sStoredUser = window.sessionStorage.getItem("pcct_test_user") || "";
            oStateModel.setProperty("/testUser", sStoredUser);
            oStateModel.setProperty("/testUserLogin", sStoredUser);
            oStateModel.setProperty("/requiresUserLogin", !sStoredUser);

            // Protect against accidental tab close while unsaved changes exist.
            this._fnBeforeUnload = function (oEvent) {
                var bHasUnsaved = oStateModel.getProperty("/mode") === "EDIT" && oStateModel.getProperty("/isDirty");
                if (!bHasUnsaved) {
                    return;
                }
                var sMsg = "You have unsaved changes";
                oEvent.preventDefault();
                oEvent.returnValue = sMsg;
                return sMsg;
            };
            window.addEventListener("beforeunload", this._fnBeforeUnload);

            oLayoutModel.setProperty("/smartFilter/fields", SmartSearchAdapter.getSmartFilterConfig().fields);
            oLayoutModel.setProperty("/smartTable/columns", SmartSearchAdapter.getSmartTableConfig().columns);
            oLayoutModel.setProperty("/smartTable/selectionMode", SmartSearchAdapter.getSmartTableConfig().selectionMode);


            oStateModel.bindProperty("/isDirty").attachChange(function () {
                this._oAutoSave.touch();
            }.bind(this));
            ["/isLocked", "/mode"].forEach(function (sPath) {
                oStateModel.bindProperty(sPath).attachChange(function () {
                    this._syncLockScopedManagers(oStateModel);
                }.bind(this));
            }.bind(this));

            this._oConnectivity.attachEvent("state", function (oEvent) {
                var m = oEvent.getParameters() || {};
                oStateModel.setProperty("/networkOnline", !!m.online);
                oStateModel.setProperty("/networkGraceMode", !!m.isGrace);
                oStateModel.setProperty("/networkGraceExpiresAt", m.graceExpiresAt || null);
            });

            this._oConnectivity.attachEvent("graceExpired", function () {
                oStateModel.setProperty("/mode", "READ");
                MessageBox.warning(this.getModel("i18n").getResourceBundle().getText("networkGraceExpired"));
            }.bind(this));

            this.getRouter().attachBeforeRouteMatched(function (oEvent) {
                if (oStateModel.getProperty("/navGuardBypass")) {
                    oStateModel.setProperty("/navGuardBypass", false);
                    return;
                }
                if (!oStateModel.getProperty("/isDirty")) {
                    return;
                }
                oEvent.preventDefault();
                FlowCoordinator.confirmUnsavedAndHandle({
                    getModel: this.getModel.bind(this),
                    getResourceBundle: function () { return this.getModel("i18n").getResourceBundle(); }.bind(this)
                }, function () { return Promise.resolve(false); }).then(function (sDecision) {
                    if (sDecision === "DISCARD" || sDecision === "SAVE" || sDecision === "NO_CHANGES") {
                        oStateModel.setProperty("/navGuardBypass", true);
                        var sName = oEvent.getParameter("name");
                        var oArgs = oEvent.getParameter("arguments") || {};
                        this.getRouter().navTo(sName, oArgs, false);
                    }
                }.bind(this));
            }.bind(this));

            if (/detail\/__create(?:$|[?&])/.test(window.location.hash || "")) {
                this.getRouter().getHashChanger().replaceHash("");
                oStateModel.setProperty("/objectAction", "");
            }

            this.getRouter().initialize();

            oStateModel.setProperty("/isLoading", true);
            oStateModel.setProperty("/masterDataLoading", true);
            oStateModel.setProperty("/locationsLoading", false);

            Promise.all([
                // Outside SAP runtime user may be injected from a lightweight modal and sessionStorage.
                BackendAdapter.login(oStateModel.getProperty("/testUser") || "demoUser"),
                BackendAdapter.init(),
                BackendAdapter.getServerState().catch(function () { return null; }),
                BackendAdapter.getFrontendConfig().catch(function () { return null; })
            ]).then(function (aResults) {
                var oLogin = aResults[0];
                var oServerState = aResults[2];
                var oFrontendConfig = aResults[3] || {};

                oStateModel.setProperty("/sessionId", oLogin.sessionId);
                if (oFrontendConfig.search && oFrontendConfig.search.defaultMaxResults) {
                    oStateModel.setProperty("/searchMaxResults", String(oFrontendConfig.search.defaultMaxResults));
                }
                this._applyFrontendValidationAndVariables(oFrontendConfig || {}, oStateModel, oEnvModel);
                this._applyFrontendRuntimeConfig(oFrontendConfig || {}, oStateModel, oEnvModel);

                this._loadMasterDataAsync(oMasterDataModel, oStateModel, oEnvModel);

                return this._oSmartCache.getWithFallback("checkLists").then(function (aCached) {
                    if (Array.isArray(aCached) && aCached.length) {
                        oDataModel.setProperty("/checkLists", aCached);
                        oDataModel.setProperty("/visibleCheckLists", aCached);
                    }
                    return BackendAdapter.getCheckLists();
                }).then(function (aCheckLists) {
                    // OData/backend is used only as transport layer; UI bindings always target JSON models.
                    oDataModel.setProperty("/checkLists", aCheckLists);
                    oDataModel.setProperty("/visibleCheckLists", aCheckLists);

                    // Cache snapshot is the local source for diff/dirty calculations.
                    oCacheModel.setProperty("/pristineSnapshot", JSON.parse(JSON.stringify(aCheckLists)));
                    var sCacheAtRaw = (oServerState && (oServerState.checkedAt || oServerState.fetchedAt)) || new Date();
                    var sCacheAt = fnFormatHumanDateTime(sCacheAtRaw);
                    oCacheModel.setProperty("/lastServerState", oServerState || {
                        fetchedAt: sCacheAt,
                        count: aCheckLists.length
                    });
                    oStateModel.setProperty("/cacheValidationAt", sCacheAt);
                    oCacheModel.setProperty("/keyMapping", this._oSmartCache.snapshot().keyMapping);
                    this._oSmartCache.put("checkLists", aCheckLists);
                }.bind(this));
            }.bind(this)).catch(function (oError) {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", "Ошибка при загрузке данных: " + oError.message);
            }).finally(function () {
                oStateModel.setProperty("/isLoading", false);
                this._oHeartbeat.start();
                this._oActivity.start();
                this._oAutoSave.start();
                this._oConnectivity.start();
                this._oLockStatus.start();
                this._syncLockScopedManagers(oStateModel);
            }.bind(this));
        },


        _isLockRuntimeActive: function (oStateModel) {
            return oStateModel.getProperty("/mode") === "EDIT" && !!oStateModel.getProperty("/isLocked");
        },

        _syncLockScopedManagers: function (oStateModel) {
            var bActive = this._isLockRuntimeActive(oStateModel);
            if (bActive) {
                if (this._oHeartbeat && !this._oHeartbeat.isRunning()) {
                    this._oHeartbeat.start();
                }
                if (this._oAutoSave) {
                    this._oAutoSave.start();
                }
                if (this._oLockStatus) {
                    this._oLockStatus.start();
                }
                if (this._oGcd) {
                    this._oGcd.resetOnFullSave();
                }
                if (this._oActivity) {
                    this._oActivity.start();
                }
                return;
            }

            if (this._oHeartbeat) {
                this._oHeartbeat.stop();
            }
            if (this._oAutoSave) {
                this._oAutoSave.stop();
            }
            if (this._oLockStatus) {
                this._oLockStatus.stop();
            }
            if (this._oGcd) {
                this._oGcd.destroyManager();
            }
            if (this._oActivity) {
                this._oActivity.stop();
            }
        },

        _sanitizeTimers: function (mIncoming, mFallback) {
            var mBase = mFallback || {};
            var mRaw = mIncoming || {};
            function pick(sKey, iDefault) {
                var iVal = Number(mRaw[sKey]);
                return Number.isFinite(iVal) && iVal >= 1000 ? iVal : iDefault;
            }
            return {
                heartbeatMs: pick("heartbeatMs", Number(mBase.heartbeatMs) || 240000),
                lockStatusMs: pick("lockStatusMs", Number(mBase.lockStatusMs) || 60000),
                gcdMs: pick("gcdMs", Number(mBase.gcdMs) || 300000),
                idleMs: pick("idleMs", Number(mBase.idleMs) || 600000),
                autoSaveIntervalMs: pick("autoSaveIntervalMs", Number(mBase.autoSaveIntervalMs) || 60000),
                autoSaveDebounceMs: pick("autoSaveDebounceMs", Number(mBase.autoSaveDebounceMs) || 30000),
                networkGraceMs: pick("networkGraceMs", Number(mBase.networkGraceMs) || 60000),
                cacheFreshMs: pick("cacheFreshMs", Number(mBase.cacheFreshMs) || 30000),
                cacheStaleOkMs: pick("cacheStaleOkMs", Number(mBase.cacheStaleOkMs) || 90000),
                analyticsRefreshMs: pick("analyticsRefreshMs", Number(mBase.analyticsRefreshMs) || 900000)
            };
        },

        _applyFrontendRuntimeConfig: function (oFrontendConfig, oStateModel, oEnvModel) {
            var mTimers = this._sanitizeTimers((oFrontendConfig && oFrontendConfig.timers) || {}, oStateModel.getProperty("/timers") || {});
            oStateModel.setProperty("/timers", mTimers);
            oEnvModel.setProperty("/source", (oFrontendConfig && oFrontendConfig.source) || "config_frontend");
            oEnvModel.setProperty("/loadedAt", new Date().toISOString());
            oEnvModel.setProperty("/timers", mTimers);

            if (this._oHeartbeat && this._oHeartbeat.setIntervalMs) {
                this._oHeartbeat.setIntervalMs(mTimers.heartbeatMs);
            }
            if (this._oLockStatus && this._oLockStatus.setIntervalMs) {
                this._oLockStatus.setIntervalMs(mTimers.lockStatusMs);
            }
            if (this._oGcd && this._oGcd.setIntervalMs) {
                this._oGcd.setIntervalMs(mTimers.gcdMs);
            }
            if (this._oActivity && this._oActivity.setIdleMs) {
                this._oActivity.setIdleMs(mTimers.idleMs);
            }
            if (this._oAutoSave && this._oAutoSave.setIntervals) {
                this._oAutoSave.setIntervals({
                    intervalMs: mTimers.autoSaveIntervalMs,
                    debounceMs: mTimers.autoSaveDebounceMs
                });
            }
            if (this._oConnectivity && this._oConnectivity.setGraceMs) {
                this._oConnectivity.setGraceMs(mTimers.networkGraceMs);
            }
            if (this._oSmartCache && this._oSmartCache.configureFreshness) {
                this._oSmartCache.configureFreshness({ freshMs: mTimers.cacheFreshMs, staleOkMs: mTimers.cacheStaleOkMs });
            }
        },


        _applyFrontendValidationAndVariables: function (oFrontendConfig, oStateModel, oEnvModel) {
            var oConfig = oFrontendConfig || {};
            var aRequired = Array.isArray(oConfig.requiredFields) ? oConfig.requiredFields.slice() : [];
            var mVars = (oConfig.variables && typeof oConfig.variables === "object") ? Object.assign({}, oConfig.variables) : {};

            oStateModel.setProperty("/requiredFields", aRequired);
            oStateModel.setProperty("/frontendVariables", mVars);
            oStateModel.setProperty("/frontendConfigSource", oConfig.source || "defaults");
            if (oEnvModel) {
                oEnvModel.setProperty("/variables", mVars);
            }
        },

        _loadMasterDataAsync: function (oMasterDataModel, oStateModel, oEnvModel) {
            var pBundle = BackendAdapter.getReferenceBundle().catch(function () {
                var pPersons = BackendAdapter.getPersons().catch(function () { return []; });
                var pLpc = BackendAdapter.getDictionary("LPC").catch(function () { return []; });
                var pProfessions = BackendAdapter.getDictionary("PROFESSION").catch(function () { return []; });
                return Promise.all([pPersons, pLpc, pProfessions]).then(function (aResults) {
                    return {
                        persons: aResults[0],
                        dictionaries: {
                            LPC: aResults[1],
                            PROFESSION: aResults[2]
                        }
                    };
                });
            });

            pBundle.then(function (oBundle) {
                var mDict = (oBundle && oBundle.dictionaries) || {};
                oMasterDataModel.setProperty("/persons", (oBundle && oBundle.persons) || []);
                oMasterDataModel.setProperty("/lpc", mDict.LPC || []);
                oMasterDataModel.setProperty("/professions", mDict.PROFESSION || []);
                if (oEnvModel && oEnvModel.setProperty) {
                    var mRuntimeVars = (oBundle && oBundle.variables && typeof oBundle.variables === "object") ? oBundle.variables : {};
                    var mCurrentVars = oEnvModel.getProperty("/variables") || {};
                    oEnvModel.setProperty("/variables", Object.assign({}, mCurrentVars, mRuntimeVars));
                    oStateModel.setProperty("/frontendVariables", Object.assign({}, mCurrentVars, mRuntimeVars));
                }
            }).finally(function () {
                oStateModel.setProperty("/masterDataLoading", false);
            });

        },


        exit: function () {
            if (this._oHeartbeat) {
                this._oHeartbeat.stop();
            }
            if (this._oGcd) {
                this._oGcd.destroyManager();
            }
            if (this._oActivity) {
                this._oActivity.stop();
            }
            if (this._oAutoSave) {
                this._oAutoSave.stop();
            }
            if (this._oConnectivity) {
                this._oConnectivity.stop();
            }
            if (this._oLockStatus) {
                this._oLockStatus.stop();
            }
            if (this._fnUnregisterBeacon) {
                this._fnUnregisterBeacon();
            }
            if (this._fnBeforeUnload) {
                window.removeEventListener("beforeunload", this._fnBeforeUnload);
            }
            if (this._fnOnFullSave) {
                window.removeEventListener("pcct:fullSave", this._fnOnFullSave);
            }
        }
    });
});
