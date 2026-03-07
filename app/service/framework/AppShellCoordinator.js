sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "checklist/app/infra/navigation/RouteModeCoordinator",
    "checklist/app/util/DebugLogger",
    "checklist/app/service/framework/AppShellStateSync",
    "checklist/app/util/TimeConfigService",
    "checklist/app/util/runtime/TimerDefaults"
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
        oController._oRouteModeCoordinator = new RouteModeCoordinator({
            router: oController.getRouter(),
            stateModel: oResolvedState,
            fcl: oController.byId("mainFcl")
        });
        oController._oRouteModeCoordinator.start();
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
        onExit: function (oController) {
            if (oController._oRouteModeCoordinator) {
                oController._oRouteModeCoordinator.stop();
                oController._oRouteModeCoordinator = null;
            }
        }
    };
});
