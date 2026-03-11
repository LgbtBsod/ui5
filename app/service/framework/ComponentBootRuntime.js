sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil"
], function (ModelStateRuntime, CloneUtil) {
    "use strict";

    function runBootSequence(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oEnvModel = mOptions.envModel;
        var oCacheModel = mOptions.cacheModel;
        var BootstrapAppUseCase = mOptions.bootstrapAppUseCase;
        var EnsureDictLoadedUseCase = mOptions.ensureDictLoadedUseCase;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var fnLoadRuntimeSettings = mOptions.loadRuntimeSettings;
        var fnLoadCurrentUser = mOptions.loadCurrentUser;
        var fnBundleText = mOptions.bundleText;
        var bBootCompleted = false;

        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": true,
            "/locationsLoading": false,
            "/readiness/app": {
                status: "loading",
                ready: false,
                readyAt: "",
                error: ""
            }
        });

        function allSettledPolyfill(aPromises) {
            return Promise.all((aPromises || []).map(function (p) {
                return Promise.resolve(p).then(
                    function (v) { return { status: "fulfilled", value: v }; },
                    function (e) { return { status: "rejected", reason: e }; }
                );
            }));
        }

        function toStageError(vError, sFallbackMessage) {
            if (vError instanceof Error) {
                return vError;
            }
            return new Error(String(vError || sFallbackMessage || "boot_stage_failed"));
        }

        function resolveSettledStageError(oSettledResult, sFallbackMessage) {
            if (!oSettledResult) {
                return toStageError(null, sFallbackMessage);
            }
            if (oSettledResult.status === "rejected") {
                return toStageError(oSettledResult.reason, sFallbackMessage);
            }
            if (oSettledResult.value && oSettledResult.value.ok === false) {
                return toStageError(oSettledResult.value.error && oSettledResult.value.error.message, sFallbackMessage);
            }
            return null;
        }

        return BootstrapAppUseCase.execute({}, { stateModel: oStateModel }).then(function (oBootstrapResult) {
            var oServerState = null;
            var sReadyAt;
            var oBootError;
            if (oBootstrapResult && oBootstrapResult.ok === false) {
                throw toStageError(oBootstrapResult.error && oBootstrapResult.error.message, "bootstrap_app_failed");
            }
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            ComponentRuntimeSupport.ensureTabSessionId(oStateModel);
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
                "/frontendVariables": {},
                "/frontendConfigSource": "gateway"
            });
            ModelStateRuntime.writeOnModel(oEnvModel, "/variables", {});
            return allSettledPolyfill([
                Promise.resolve(typeof fnLoadCurrentUser === "function" ? fnLoadCurrentUser() : null),
                Promise.resolve(typeof fnLoadRuntimeSettings === "function" ? fnLoadRuntimeSettings() : null),
                Promise.resolve(EnsureDictLoadedUseCase.execute({}, oComponent._ctx))
            ]).then(function (aStageResults) {
                oBootError = resolveSettledStageError(aStageResults[0], "load_current_user_failed") ||
                    resolveSettledStageError(aStageResults[1], "load_runtime_settings_failed") ||
                    resolveSettledStageError(aStageResults[2], "bootstrap_init_bundle_failed");
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
