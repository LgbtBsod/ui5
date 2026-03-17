sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeCoordinator",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimerDefaults",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
], function (JSONModel, RouteModeCoordinator, DebugLogger, ControllerModelRuntime, ModelStateRuntime, TimeConfigService, TimerDefaults, SchedulingRuntime, NavigationContracts) {
    "use strict";

    var LAYOUTS = NavigationContracts.LAYOUTS;

    function resolveStateModel(oController) {
        return ControllerModelRuntime.state(oController);
    }

    function ensureControllerStateModel(oController, oStateModel) {
        if (oStateModel && !ControllerModelRuntime.state(oController)) {
            ControllerModelRuntime.view(oController).setModel(oStateModel, "state");
        }
    }

    function syncThemeState(oController, sSource, oThemeResult) {
        var oAppView = ControllerModelRuntime.appView(oController);
        var sStoredTheme = oController.getCurrentTheme();
        var sMode = String((oThemeResult && oThemeResult.mode) || "morning").trim().toLowerCase() || "morning";
        var bIsDark = false;
        var bAnimationEnabled = !oThemeResult || oThemeResult.animationEnabled !== false;
        if (oAppView) {
            ModelStateRuntime.setManyOnModel(oAppView, {
                "/isDark": bIsDark,
                "/themeMode": sMode,
                "/animationEnabled": bAnimationEnabled
            });
        }
        DebugLogger.info("theme", "sync", {
            source: sSource || "unknown",
            storedTheme: sStoredTheme,
            appliedTheme: oThemeResult && oThemeResult.theme,
            isDark: bIsDark,
            animationEnabled: bAnimationEnabled,
            expectedIcon: "sun"
        });
    }

    function initStateBoundUi(oController, oStateModel) {
        if (oController._oRouteModeCoordinator) {
            return;
        }
        var oResolvedState = oStateModel || resolveStateModel(oController);
        if (!oResolvedState) {
            SchedulingRuntime.restartTimer(0, function () {
                initStateBoundUi(oController);
            }, Number((TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50));
            return;
        }
        var iBootstrapRetryMs = Number(TimeConfigService.read(oResolvedState, "bootstrapRetryMs") || (TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50);
        ensureControllerStateModel(oController, oResolvedState);
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
                invertedBlockScheme: false
            }), "appView");
            syncThemeState(oController, "init", oApplied);
            var oState = resolveStateModel(oController);
            ensureControllerStateModel(oController, oState);
            if (oState) {
                ModelStateRuntime.setManyOnModel(oState, {
                    "/layout": ModelStateRuntime.readOnModel(oState, "/layout", LAYOUTS.ONE_COLUMN) || LAYOUTS.ONE_COLUMN,
                    "/selectedId": typeof ModelStateRuntime.readOnModel(oState, "/selectedId", undefined) === "undefined" ? null : ModelStateRuntime.readOnModel(oState, "/selectedId", null)
                });
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
        onExit: function (oController) {
            if (oController._oRouteModeCoordinator) {
                oController._oRouteModeCoordinator.stop();
                oController._oRouteModeCoordinator = null;
            }
        }
    };
});
