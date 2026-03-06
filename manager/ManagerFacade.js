sap.ui.define([
    "sap_ui5/manager/SmartCacheManager",
    "sap_ui5/manager/HeartbeatManager",
    "sap_ui5/manager/GCDManager",
    "sap_ui5/manager/ActivityMonitor",
    "sap_ui5/manager/AutoSaveCoordinator",
    "sap_ui5/manager/ConnectivityCoordinator",
    "sap_ui5/manager/LockStatusMonitor",
    "sap_ui5/manager/SettingsManager"
], function (
    SmartCacheManager,
    HeartbeatManager,
    GCDManager,
    ActivityMonitor,
    AutoSaveCoordinator,
    ConnectivityCoordinator,
    LockStatusMonitor,
    SettingsManager
) {
    "use strict";

    return {
        SmartCacheManager: SmartCacheManager,
        HeartbeatManager: HeartbeatManager,
        GCDManager: GCDManager,
        ActivityMonitor: ActivityMonitor,
        AutoSaveCoordinator: AutoSaveCoordinator,
        ConnectivityCoordinator: ConnectivityCoordinator,
        LockStatusMonitor: LockStatusMonitor,
        SettingsManager: SettingsManager
    };
});
