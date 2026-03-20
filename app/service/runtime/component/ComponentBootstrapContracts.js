sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        GROUPS: Object.freeze({
            CORE: "core",
            MANAGERS: "managers",
            MODELS: "models",
            RUNTIME: "runtime",
            TELEMETRY: "telemetry",
            THEME: "theme",
            USECASES: "usecases"
        }),
        MANAGER_KEYS: Object.freeze({
            ACTIVITY_MONITOR: "ActivityMonitor",
            AUTOSAVE_COORDINATOR: "AutoSaveCoordinator",
            GCD_MANAGER: "GCDManager",
            HEARTBEAT_MANAGER: "HeartbeatManager",
            LOCK_STATUS_MONITOR: "LockStatusMonitor"
        })
    });
});
