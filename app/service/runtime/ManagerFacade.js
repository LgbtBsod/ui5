sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/HeartbeatManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/GCDManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ActivityMonitor",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/AutoSaveCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ConnectivityCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/LockStatusMonitor",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/SettingsManager"
], function (
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
        HeartbeatManager: HeartbeatManager,
        GCDManager: GCDManager,
        ActivityMonitor: ActivityMonitor,
        AutoSaveCoordinator: AutoSaveCoordinator,
        ConnectivityCoordinator: ConnectivityCoordinator,
        LockStatusMonitor: LockStatusMonitor,
        SettingsManager: SettingsManager
    };
});
