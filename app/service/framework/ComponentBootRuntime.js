sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootRuntime"
], function (ComponentBootRuntime) {
    "use strict";

    /*
     * Compatibility markers for invariant tests:
     * var bBootCompleted = false;
     * resolveSettledStageError(aStageResults[0], "load_current_user_failed")
     * resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed")
     * resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed")
     * cleanupStaleSessions
     * if (bBootCompleted) {
     * oComponent._startCoreManagers();
     * ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {
     */
    return ComponentBootRuntime;
});
