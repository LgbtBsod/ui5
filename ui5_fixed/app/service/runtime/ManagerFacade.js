sap.ui.define([
    "checklist/app/service/runtime/SmartCacheManager",
    "checklist/app/service/runtime/HeartbeatManager",
    "checklist/app/service/runtime/GCDManager",
    "checklist/app/service/runtime/ActivityMonitor",
    "checklist/app/service/runtime/AutoSaveCoordinator",
    "checklist/app/service/runtime/ConnectivityCoordinator",
    "checklist/app/service/runtime/LockStatusMonitor",
    "checklist/app/service/runtime/SettingsManager"
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
