sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap_ui5/model/ModelFactory",
    "sap_ui5/service/ChecklistService",
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter",
    "sap_ui5/manager/SmartCacheManager",
    "sap_ui5/manager/HeartbeatManager",
    "sap_ui5/manager/GCDManager",
    "sap_ui5/manager/ActivityMonitor",
    "sap_ui5/manager/BeaconManager",
    "sap/ui/model/json/JSONModel"
], function (
    UIComponent,
    ModelFactory,
    ChecklistService,
    BackendAdapter,
    SmartSearchAdapter,
    SmartCacheManager,
    HeartbeatManager,
    GCDManager,
    ActivityMonitor,
    BeaconManager,
    JSONModel
) {
    "use strict";

    return UIComponent.extend("sap_ui5.Component", {

        metadata: {
            manifest: "json"
        },

        init: function () {
            UIComponent.prototype.init.apply(this, arguments);

            var oDataModel = ModelFactory.createDataModel();
            var oStateModel = ModelFactory.createStateModel();
            var oLayoutModel = ModelFactory.createLayoutModel();
            var oCacheModel = ModelFactory.createCacheModel();
            var oMasterDataModel = ModelFactory.createMasterDataModel();
            var oMplModel = ModelFactory.createMplModel();

            this._oSmartCache = new SmartCacheManager();
            this._oHeartbeat = new HeartbeatManager({
                heartbeatFn: function () {
                    return BackendAdapter.lockHeartbeat(oStateModel.getProperty("/sessionId"));
                }
            });
            this._oGcd = new GCDManager();
            this._oActivity = new ActivityMonitor();
            this._fnUnregisterBeacon = BeaconManager.register(function () {
                BackendAdapter.lockRelease(oStateModel.getProperty("/sessionId"));
            });

            this._oHeartbeat.attachEvent("heartbeat", function (oEvent) {
                var oPayload = oEvent.getParameters() || {};
                oStateModel.setProperty("/isKilled", !!oPayload.is_killed);
                oCacheModel.setProperty("/lastServerState", {
                    lastChangeSet: oPayload.last_change_set || null,
                    serverChangedOn: oPayload.server_changed_on || null,
                    checkedAt: new Date().toISOString()
                });
            });

            this._oGcd.attachEvent("gcdExpired", function () {
                oStateModel.setProperty("/hasConflict", true);
            });

            this._oActivity.attachEvent("idleTimeout", function () {
                oStateModel.setProperty("/idleExpires", new Date().toISOString());
                oStateModel.setProperty("/mode", "READ");
            });

            this.setModel(new JSONModel({}), "selected");
            this.setModel(oDataModel, "data");
            this.setModel(oStateModel, "state");
            this.setModel(oLayoutModel, "layout");
            this.setModel(oCacheModel, "cache");
            this.setModel(oMasterDataModel, "masterData");
            this.setModel(oMplModel, "mpl");

            oLayoutModel.setProperty("/smartFilter/fields", SmartSearchAdapter.getSmartFilterConfig().fields);
            oLayoutModel.setProperty("/smartTable/columns", SmartSearchAdapter.getSmartTableConfig().columns);
            oLayoutModel.setProperty("/smartTable/selectionMode", SmartSearchAdapter.getSmartTableConfig().selectionMode);

            this.getRouter().initialize();

            var sStoredTheme = window.localStorage.getItem("sap_ui5_theme") || "dark";
            document.body.classList.toggle("appDark", sStoredTheme !== "light");
            document.body.classList.toggle("appLight", sStoredTheme === "light");
            document.documentElement.classList.toggle("light-mode", sStoredTheme === "light");

            oStateModel.setProperty("/isLoading", true);

            Promise.all([
                BackendAdapter.login("demoUser"),
                BackendAdapter.init(),
                ChecklistService.loadPersons().catch(function () { return []; }),
                ChecklistService.loadLpc().catch(function () { return []; }),
                ChecklistService.loadProfessions().catch(function () { return []; }),
                ChecklistService.loadLocations().catch(function () { return []; }),
                BackendAdapter.getServerState().catch(function () { return null; })
            ]).then(function (aResults) {
                var oLogin = aResults[0];
                var aPersons = aResults[2];
                var aLpc = aResults[3];
                var aProfessions = aResults[4];
                var aLocations = aResults[5];
                var oServerState = aResults[6];

                oStateModel.setProperty("/sessionId", oLogin.sessionId);

                return BackendAdapter.getCheckLists().then(function (aCheckLists) {
                    oDataModel.setProperty("/checkLists", aCheckLists);
                    oDataModel.setProperty("/visibleCheckLists", aCheckLists);

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
            if (this._fnUnregisterBeacon) {
                this._fnUnregisterBeacon();
            }
        }
    });
});
