sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "sap_ui5/infra/navigation/RouteModeCoordinator",
    "sap_ui5/util/DebugLogger",
    "sap_ui5/service/framework/AppShellStateSync",
    "sap_ui5/util/TimeConfigService",
    "sap_ui5/util/runtime/TimerDefaults"
], function (JSONModel, RouteModeCoordinator, DebugLogger, AppShellStateSync, TimeConfigService, TimerDefaults) {
    "use strict";

    function syncThemeState(oController, sSource, oThemeResult) {
        var oAppView = oController.getView().getModel("appView");
        var sStoredTheme = oController.getCurrentTheme();
        var bIsDark = !!(oThemeResult && oThemeResult.isDark);
        var bAnimationEnabled = !oThemeResult || oThemeResult.animationEnabled !== false;
        var bBackgroundInteractive = !oThemeResult || oThemeResult.backgroundInteractive !== false;
        if (oAppView) {
            oAppView.setProperty("/isDark", bIsDark);
            oAppView.setProperty("/themeMode", bIsDark ? "night" : "morning");
            oAppView.setProperty("/animationEnabled", bAnimationEnabled);
            oAppView.setProperty("/backgroundInteractive", bBackgroundInteractive);
        }
        DebugLogger.info("theme", "sync", {
            source: sSource || "unknown",
            storedTheme: sStoredTheme,
            appliedTheme: oThemeResult && oThemeResult.theme,
            isDark: bIsDark,
            animationEnabled: bAnimationEnabled,
            backgroundInteractive: bBackgroundInteractive,
            expectedIcon: bIsDark ? "sun" : "moon"
        });
    }

    function initStateBoundUi(oController, oStateModel) {
        if (oController._oRouteModeCoordinator) {
            return;
        }
        var oResolvedState = oStateModel || AppShellStateSync.resolveStateModel(oController);
        if (!oResolvedState) {
            setTimeout(function () {
                initStateBoundUi(oController);
            }, Number((TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50));
            return;
        }
        var iBootstrapRetryMs = Number(TimeConfigService.read(oResolvedState, "bootstrapRetryMs") || (TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50);
        AppShellStateSync.ensureControllerStateModel(oController, oResolvedState);
        if (!oController._syncTestUserDialogStateBound) {
            oController._syncTestUserDialogStateBound = function () {
                AppShellStateSync.syncTestUserDialogState(oController);
            };
        }
        if (!oController._oRequiresLoginBinding) {
            oController._oRequiresLoginBinding = oResolvedState.bindProperty("/requiresUserLogin");
            oController._oRequiresLoginBinding.attachChange(oController._syncTestUserDialogStateBound);
        }
        oController._oRouteModeCoordinator = new RouteModeCoordinator({
            router: oController.getRouter(),
            stateModel: oResolvedState,
            fcl: oController.byId("mainFcl")
        });
        oController._oRouteModeCoordinator.start();
        AppShellStateSync.syncTestUserDialogState(oController);
        oController._iBootstrapRetryMs = iBootstrapRetryMs;
    }

    return {
        onInit: function (oController) {
            var oApplied = oController.applyStoredTheme();
            oController.setModel(new JSONModel({
                isDark: !!(oApplied && oApplied.isDark),
                themeMode: (oApplied && oApplied.mode) || "morning",
                animationEnabled: !oApplied || oApplied.animationEnabled !== false,
                backgroundInteractive: !oApplied || oApplied.backgroundInteractive !== false
            }), "appView");
            syncThemeState(oController, "init", oApplied);
            var oState = AppShellStateSync.resolveStateModel(oController);
            AppShellStateSync.ensureControllerStateModel(oController, oState);
            if (oState) {
                if (!oState.getProperty("/layout")) {
                    oState.setProperty("/layout", "OneColumn");
                }
                if (typeof oState.getProperty("/selectedId") === "undefined") {
                    oState.setProperty("/selectedId", null);
                }
            }
            initStateBoundUi(oController, oState);
        },
        onSetThemeMode: function (oController, sMode) {
            syncThemeState(oController, "settings-theme-mode", oController.setThemeMode(sMode));
        },
        onToggleTheme: function (oController, oClickXY) {
            syncThemeState(oController, "toggle", oController.toggleTheme(oClickXY));
        },
        onToggleThemeAnimation: function (oController, bEnabled) {
            syncThemeState(oController, "animation", oController.setThemeAnimationEnabled(!!bEnabled));
        },
        onToggleBackgroundInteractive: function (oController, bEnabled) {
            syncThemeState(oController, "background-interactive", oController.setThemeBackgroundInteractive(!!bEnabled));
        },
        requestTestUserDialog: function (oController) {
            var oState = oController && oController.getModel && oController.getModel("state");
            if (oState && typeof oState.setProperty === "function") {
                oState.setProperty("/requiresUserLogin", true);
            }
            AppShellStateSync.syncTestUserDialogState(oController);
        },
        onConfirmTestUser: function (oController, fnConfirm) {
            return fnConfirm(oController).then(function (bSuccess) {
                if (bSuccess) {
                    AppShellStateSync.closeTestUserDialog(oController);
                }
            });
        },
        onDialogClosed: function (oController) {
            AppShellStateSync.syncTestUserDialogState(oController);
        },
        onExit: function (oController) {
            if (oController._oRouteModeCoordinator) {
                oController._oRouteModeCoordinator.stop();
                oController._oRouteModeCoordinator = null;
            }
            if (oController._oRequiresLoginBinding) {
                oController._oRequiresLoginBinding.detachChange(oController._syncTestUserDialogStateBound);
                oController._oRequiresLoginBinding.destroy();
                oController._oRequiresLoginBinding = null;
            }
            oController._syncTestUserDialogStateBound = null;
        }
    };
});
