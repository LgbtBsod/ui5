sap.ui.define([], function () {
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

        oStateModel.setProperty("/isLoading", true);
        oStateModel.setProperty("/masterDataLoading", true);
        oStateModel.setProperty("/locationsLoading", false);

        return BootstrapAppUseCase.execute({}, { stateModel: oStateModel }).then(function () {
            var oServerState = null;
            ComponentRuntimeSupport.ensureSessionId(oStateModel);
            oStateModel.setProperty("/testUser", "");
            oStateModel.setProperty("/testUserLogin", "");
            oStateModel.setProperty("/currentUser", {
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
            oStateModel.setProperty("/requiresUserLogin", false);
            var aRequired = [];
            var mVars = {};
            oStateModel.setProperty("/requiredFields", aRequired);
            oStateModel.setProperty("/frontendVariables", mVars);
            oStateModel.setProperty("/frontendConfigSource", "gateway");
            oEnvModel.setProperty("/variables", mVars);
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
                oCacheModel.setProperty("/pristineSnapshot", JSON.parse(JSON.stringify(aCheckLists)));
                var sCacheAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
                oCacheModel.setProperty("/lastServerState", oServerState || {
                    fetchedAt: sCacheAt,
                    count: aCheckLists.length
                });
                oStateModel.setProperty("/cacheValidationAt", sCacheAt);
                oCacheModel.setProperty("/keyMapping", oComponent._oSmartCache.snapshot().keyMapping);
                oComponent._oSmartCache.put("checkLists", aCheckLists);
            }).catch(function (oError) {
                oStateModel.setProperty("/loadError", true);
                oStateModel.setProperty("/loadErrorMessage", fnBundleText("loadErrorMessage") + ": " + oError.message);
            });
        }).catch(function (oError) {
            oStateModel.setProperty("/loadError", true);
            oStateModel.setProperty("/loadErrorMessage", fnBundleText("loadErrorMessage") + ": " + oError.message);
        }).finally(function () {
            oStateModel.setProperty("/isLoading", false);
            oComponent._startCoreManagers();
            oComponent._syncLockScopedManagers(oStateModel);
        });
    }

    return {
        run: run
    };
});
