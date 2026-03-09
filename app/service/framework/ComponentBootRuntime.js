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

        ModelStateRuntime.setManyOnModel(oStateModel, {
            "/isLoading": true,
            "/masterDataLoading": true,
            "/locationsLoading": false
        });

        return BootstrapAppUseCase.execute({}, { stateModel: oStateModel }).then(function () {
            var oServerState = null;
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            ModelStateRuntime.writeOnModel(oStateModel, "/currentUser", {
                uname: "",
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
            Promise.resolve().then(function () {
                function allSettledPolyfill(aPromises) {
                    return Promise.all((aPromises || []).map(function (p) {
                        return Promise.resolve(p).then(
                            function (v) { return { status: "fulfilled", value: v }; },
                            function (e) { return { status: "rejected", reason: e }; }
                        );
                    }));
                }
                return allSettledPolyfill([
                    Promise.resolve(typeof fnLoadCurrentUser === "function" ? fnLoadCurrentUser() : null),
                    fnLoadRuntimeSettings(),
                    Promise.resolve(EnsureDictLoadedUseCase.execute({}, oComponent._ctx)).catch(function () {
                        return null;
                    })
                ]);
            }).then(function () {
                var aCheckLists = [];
                ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                ModelStateRuntime.setManyOnModel(oCacheModel, {
                    "/lastServerState": oServerState || {
                        fetchedAt: sCacheAt,
                        count: aCheckLists.length
                    },
                    "/keyMapping": {}
                });
                ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCacheAt);
            }).catch(function (oError) {
                ModelStateRuntime.setManyOnModel(oStateModel, {
                    "/loadError": true,
                    "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + oError.message
                });
            });
        }).catch(function (oError) {
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/loadError": true,
                "/loadErrorMessage": fnBundleText("loadErrorMessage") + ": " + oError.message
            });
        }).finally(function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/isLoading", false);
            oComponent._startCoreManagers();
            oComponent._syncLockScopedManagers(oStateModel);
        });
    }

    return {
        runBootSequence: runBootSequence
    };
});
