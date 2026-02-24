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
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap_ui5/util/FlowCoordinator"
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
    JSONModel,
    MessageBox,
    FlowCoordinator
) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            UIComponent.prototype.init.apply(this, arguments);

            BackendAdapter.configure({
                mode: this.getManifestEntry("/sap.ui5/config/backendMode") || "fake",
                baseUrl: this.getManifestEntry("/sap.app/dataSources/mockGateway/uri") || "http://localhost:8000"
            });

            var oDataModel = ModelFactory.createDataModel();
            var oStateModel = ModelFactory.createStateModel();
            var oLayoutModel = ModelFactory.createLayoutModel();
            var oCacheModel = ModelFactory.createCacheModel();
            var oMasterDataModel = ModelFactory.createMasterDataModel();
            var oMplModel = ModelFactory.createMplModel();

            this._oSmartCache = new SmartCacheManager();
            this._oHeartbeat = new HeartbeatManager({
                intervalMs: 4 * 60 * 1000,
                heartbeatFn: function () {
                    return BackendAdapter.lockHeartbeat(
                        oStateModel.getProperty("/activeObjectId"),
                        oStateModel.getProperty("/sessionId")
                    );
                }
            });
            this._oGcd = new GCDManager({ intervalMs: 5 * 60 * 1000 });
            this._oActivity = new ActivityMonitor();
            this._oAutoSave = new AutoSaveCoordinator({
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
                    var oPayload = (oObj && oObj.root && oObj.root.id === sId) ? oObj : oSelected;
                    if (!sId || !oPayload || !oPayload.root) {
                        return null;
                    }
                    return { id: sId, payload: JSON.parse(JSON.stringify(oPayload)) };
                }.bind(this),
                saveFn: function (oPayload) {
                    return BackendAdapter.updateCheckList(oPayload.id, oPayload.payload, { force: false }).then(function (oSaved) {
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
            this._oConnectivity = new ConnectivityCoordinator({ graceMs: 60 * 1000 });
            this._fnUnregisterBeacon = BeaconManager.register(function () {
                return BackendAdapter.buildReleaseBeaconPayload(
                    oStateModel.getProperty("/activeObjectId"),
                    oStateModel.getProperty("/sessionId"),
                    { trySave: true }
                );
            });

            this._oHeartbeat.attachEvent("heartbeat", function (oEvent) {
                var oPayload = oEvent.getParameters() || {};
                var bKilled = !!oPayload.is_killed;
                oStateModel.setProperty("/isKilled", bKilled);
                oStateModel.setProperty("/lockExpires", oPayload.lock_expires || null);
                if (bKilled) {
                    oStateModel.setProperty("/mode", "READ");
                    oStateModel.setProperty("/isLocked", false);
                    this._oHeartbeat.stop();
                    this._oGcd.destroyManager();
                    MessageBox.warning(this.getModel("i18n").getResourceBundle().getText("lockKilledMessage"));
                    FlowCoordinator.releaseWithTrySave({ getModel: this.getModel.bind(this), getResourceBundle: function(){ return this.getModel("i18n").getResourceBundle();}.bind(this) });
                }
                oCacheModel.setProperty("/lastServerState", {
                    lastChangeSet: oPayload.last_change_set || null,
                    serverChangedOn: oPayload.server_changed_on || null,
                    checkedAt: new Date().toISOString()
                });
            }.bind(this));


            this._oHeartbeat.attachEvent("heartbeatError", function () {
                oStateModel.setProperty("/hasConflict", true);
            });

            this._oGcd.attachEvent("gcdExpired", function () {
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
            this.setModel(oLayoutModel, "layout");
            this.setModel(oCacheModel, "cache");
            this.setModel(oMasterDataModel, "masterData");
            this.setModel(oMplModel, "mpl");

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
                    if (sDecision === "DISCARD" || sDecision === "NO_CHANGES") {
                        oStateModel.setProperty("/navGuardBypass", true);
                        var sName = oEvent.getParameter("name");
                        var oArgs = oEvent.getParameter("arguments") || {};
                        this.getRouter().navTo(sName, oArgs, false);
                    }
                }.bind(this));
            }.bind(this));

            this.getRouter().initialize();

            oStateModel.setProperty("/isLoading", true);

            Promise.all([
                // Outside SAP runtime user may be injected from a lightweight modal and sessionStorage.
                BackendAdapter.login(oStateModel.getProperty("/testUser") || "demoUser"),
                BackendAdapter.init(),
                BackendAdapter.getPersons().catch(function () { return []; }),
                BackendAdapter.getDictionary("LPC").catch(function () { return []; }),
                BackendAdapter.getDictionary("PROFESSION").catch(function () { return []; }),
                BackendAdapter.getLocations().catch(function () { return []; }),
                BackendAdapter.getServerState().catch(function () { return null; })
            ]).then(function (aResults) {
                var oLogin = aResults[0];
                var aPersons = aResults[2];
                var aLpc = aResults[3];
                var aProfessions = aResults[4];
                var aLocations = aResults[5];
                var oServerState = aResults[6];

                oStateModel.setProperty("/sessionId", oLogin.sessionId);

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
                    oCacheModel.setProperty("/lastServerState", oServerState || {
                        fetchedAt: new Date().toISOString(),
                        count: aCheckLists.length
                    });
                    oCacheModel.setProperty("/keyMapping", this._oSmartCache.snapshot().keyMapping);
                    this._oSmartCache.put("checkLists", aCheckLists);
                }.bind(this)).then(function () {
                    oMasterDataModel.setProperty("/persons", aPersons);
                    oMasterDataModel.setProperty("/lpc", aLpc);
                    oMasterDataModel.setProperty("/professions", aProfessions);
                    oMplModel.setProperty("/locations", aLocations);
                });
            }.bind(this)).catch(function (oError) {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", "Ошибка при загрузке данных: " + oError.message);
            }).finally(function () {
                oStateModel.setProperty("/isLoading", false);
                this._oHeartbeat.start();
                this._oActivity.start();
                this._oAutoSave.start();
                this._oConnectivity.start();
            }.bind(this));
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
