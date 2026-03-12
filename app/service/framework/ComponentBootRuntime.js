sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/util/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/util/runtime/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootStageRuntime"
], function (ModelStateRuntime, CloneUtil, WorkflowTelemetry, FrontendConfigConstants, ComponentBootStageRuntime) {
    "use strict";

    function runBootSequence(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oEnvModel = mOptions.envModel;
        var oCacheModel = mOptions.cacheModel;
        var BootstrapAppUseCase = mOptions.bootstrapAppUseCase;
        var EnsureDictLoadedUseCase = mOptions.ensureDictLoadedUseCase;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var oCacheAdapter = mOptions.cacheAdapter;
        var fnLoadRuntimeSettings = mOptions.loadRuntimeSettings;
        var fnLoadCurrentUser = mOptions.loadCurrentUser;
        var fnBundleText = mOptions.bundleText;
        var bBootCompleted = false;
        var sTabSessionId = "";

        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": false,
            "/locationsLoading": false,
            "/readiness/app": {
                status: "loading",
                ready: false,
                readyAt: "",
                error: ""
            }
        });

        return BootstrapAppUseCase.execute({}, { stateModel: oStateModel }).then(function (oBootstrapResult) {
            var oServerState = null;
            var sReadyAt;
            var oBootError;
            if (oBootstrapResult && oBootstrapResult.ok === false) {
                throw ComponentBootStageRuntime.toStageError(oBootstrapResult.error && oBootstrapResult.error.message, "bootstrap_app_failed");
            }
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            sTabSessionId = ComponentRuntimeSupport.ensureTabSessionId(oStateModel);
            ModelStateRuntime.writeOnModel(oStateModel, "/currentUser", {
                fullName: "",
                permissions: [],
                permissionRules: [],
                canView: false,
                canEdit: false,
                canDelete: false,
                summaryText: "",
                fetchedAt: ""
            });
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/requiredFields": [],
                "/frontendVariables": Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES),
                "/frontendConfigSource": "gateway"
            });
            ModelStateRuntime.writeOnModel(oEnvModel, "/variables", Object.assign({}, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES));
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
                return ComponentBootStageRuntime.allSettledPolyfill([
                Promise.resolve(typeof fnLoadCurrentUser === "function" ? fnLoadCurrentUser() : null),
                Promise.resolve(typeof fnLoadRuntimeSettings === "function" ? fnLoadRuntimeSettings() : null),
                Promise.resolve(EnsureDictLoadedUseCase.execute({}, oComponent._ctx))
                ]);
            }).then(function (aStageResults) {
                oBootError = ComponentBootStageRuntime.resolveSettledStageError(aStageResults[0], "load_current_user_failed") ||
                    ComponentBootStageRuntime.resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed") ||
                    ComponentBootStageRuntime.resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed");
                if (oBootError) {
                    throw oBootError;
                }
                var aCheckLists = [];
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                sReadyAt = new Date().toISOString();
                ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
                ModelStateRuntime.setManyOnModel(oCacheModel, {
                    "/lastServerState": oServerState || {
                        fetchedAt: sCacheAt,
                        count: aCheckLists.length
                    },
                    "/keyMapping": {}
                });
                ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCacheAt);
                ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {
                    status: "ready",
                    ready: true,
                    readyAt: sReadyAt,
                    error: ""
                });
                WorkflowTelemetry.emit("boot.readiness.ready", {
                    stateModel: oStateModel,
                    payload: {
                        readyAt: sReadyAt,
                        activeTabSessionId: sTabSessionId
                    }
                });
                bBootCompleted = true;
            });
        }).catch(function (oError) {
            var sErrorMessage = String((oError && oError.message) || oError || "boot_failed");
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/loadError": true,
                "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + sErrorMessage
            });
            ModelStateRuntime.writeOnModel(oStateModel, "/readiness/app", {
                status: "error",
                ready: false,
                readyAt: "",
                error: sErrorMessage
            });
            WorkflowTelemetry.emit("boot.readiness.error", {
                stateModel: oStateModel,
                payload: {
                    error: sErrorMessage,
                    activeTabSessionId: sTabSessionId
                }
            });
            return null;
        }).finally(function () {
            if (bBootCompleted) {
                oComponent._startCoreManagers();
                oComponent._syncLockScopedManagers(oStateModel);
            }
            ModelStateRuntime.writeOnModel(oStateModel, "/isLoading", false);
        });
    }

    return {
        runBootSequence: runBootSequence
    };
});
