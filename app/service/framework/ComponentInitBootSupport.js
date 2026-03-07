sap.ui.define([
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/util/CloneUtil"
], function (ModelStateRuntime, CloneUtil) {
    "use strict";

    function run(mOptions) {
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
            var aRequired = [];
            var mVars = {};
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/requiredFields": aRequired,
                "/frontendVariables": mVars,
                "/frontendConfigSource": "gateway"
            });
            ModelStateRuntime.writeOnModel(oEnvModel, "/variables", mVars);
            Promise.resolve().then(function () {
                return Promise.allSettled([
                    Promise.resolve(typeof fnLoadCurrentUser === "function" ? fnLoadCurrentUser() : null),
                    fnLoadRuntimeSettings(),
                    Promise.resolve(EnsureDictLoadedUseCase.execute({}, oComponent._ctx)).catch(function () {
                        return null;
                    }),
                    oComponent._oSmartCache.getCached("checkLists")
                ]);
            }).then(function (aResults) {
                var oCacheResult = Array.isArray(aResults) ? aResults[3] : null;
                var aCheckLists = (oCacheResult && oCacheResult.status === "fulfilled" && oCacheResult.value) || [];
                aCheckLists = Array.isArray(aCheckLists) ? aCheckLists : [];
                // Cache snapshot remains available for detail-diff flows.
                ModelStateRuntime.writeOnModel(oCacheModel, "/pristineSnapshot", CloneUtil.clone(aCheckLists, []));
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                ModelStateRuntime.setManyOnModel(oCacheModel, {
                    "/lastServerState": oServerState || {
                        fetchedAt: sCacheAt,
                        count: aCheckLists.length
                    },
                    "/keyMapping": oComponent._oSmartCache.snapshot().keyMapping
                });
                ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCacheAt);
                oComponent._oSmartCache.put("checkLists", aCheckLists);
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
        run: run
    };
});
