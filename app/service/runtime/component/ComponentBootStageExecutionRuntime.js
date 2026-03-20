sap.ui.define([
"PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootStageRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootContracts"
], function (WorkflowTelemetry, ComponentBootStageRuntime, ComponentBootContracts) {
    "use strict";

    var STAGE_ERRORS = ComponentBootContracts.STAGE_ERRORS;

    function cleanupSessions(oCacheAdapter, oStateModel, sTabSessionId) {
        return Promise.resolve(
            oCacheAdapter && typeof oCacheAdapter.cleanupStaleSessions === "function"
                ? oCacheAdapter.cleanupStaleSessions(sTabSessionId)
                : null
        ).then(function (oCleanupResult) {
            if (oCleanupResult) {
                WorkflowTelemetry.emit("cache.session.cleanup", {
                    stateModel: oStateModel,
                    payload: {
                        activeTabSessionId: sTabSessionId,
                        cleared: Number(oCleanupResult.cleared || 0) || 0
                    }
                });
            }
            return oCleanupResult;
        });
    }

    function runBootStages(mOptions) {
        return ComponentBootStageRuntime.allSettledPolyfill([
            Promise.resolve(typeof mOptions.loadCurrentUser === "function" ? mOptions.loadCurrentUser() : null),
            Promise.resolve(typeof mOptions.loadRuntimeSettings === "function" ? mOptions.loadRuntimeSettings() : null),
            Promise.resolve(mOptions.ensureDictLoadedUseCase.execute({}, mOptions.component._ctx))
        ]).then(function (aStageResults) {
            var oBootError = ComponentBootStageRuntime.resolveSettledStageError(aStageResults[0], STAGE_ERRORS.LOAD_CURRENT_USER_FAILED) ||
                ComponentBootStageRuntime.resolveSettledStageError(aStageResults[1], STAGE_ERRORS.LOAD_RUNTIME_SETTINGS_FAILED) ||
                ComponentBootStageRuntime.resolveSettledStageError(aStageResults[2], STAGE_ERRORS.BOOTSTRAP_INIT_BUNDLE_FAILED);
            if (oBootError) {
                throw oBootError;
            }
            return aStageResults;
        });
    }

    return {
        cleanupSessions: cleanupSessions,
        runBootStages: runBootStages
    };
});
