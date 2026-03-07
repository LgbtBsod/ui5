sap.ui.define([
    "checklist/app/manager/SmartCacheManager",
    "checklist/app/manager/HeartbeatManager",
    "checklist/app/manager/GCDManager",
    "checklist/app/manager/ActivityMonitor",
    "checklist/app/manager/AutoSaveCoordinator",
    "checklist/app/manager/ConnectivityCoordinator",
    "checklist/app/manager/LockStatusMonitor",
    "checklist/app/manager/SettingsManager"
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
