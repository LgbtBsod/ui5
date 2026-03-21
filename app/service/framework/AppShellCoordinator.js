sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimerDefaults",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (
    RouteModeCoordinator,
    DebugLogger,
    ControllerModelRuntime,
    ModelStateRuntime,
    ThemeContracts,
    TimeConfigService,
    TimerDefaults,
    SchedulingRuntime,
    NavigationContracts,
    ModelPathContracts,
    ModelContracts,
    JsRuntime
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SHELL_PATHS = ThemeContracts.SHELL_PATHS;
    var LAYOUTS = NavigationContracts.LAYOUTS;
    var SYNC_SOURCES = ThemeContracts.SYNC_SOURCES;
    var TYPE_UNDEFINED = JsRuntime.TYPEOF.UNDEFINED;

    function resolveStateModel(oController) {
        return ControllerModelRuntime.state(oController);
    }

    function ensureControllerStateModel(oController, oStateModel) {
        if (oStateModel && !ControllerModelRuntime.state(oController)) {
            ControllerModelRuntime.view(oController).setModel(oStateModel, MODELS.STATE);
        }
    }

    function buildShellModelData(oThemeResult) {
        return {
            animationEnabled: !oThemeResult || oThemeResult.animationEnabled !== false,
            invertedBlockScheme: false,
            // Productive UI5 1.71 landscape currently runs in safe morning-only mode.
            themeMode: ThemeContracts.MODES.MORNING
        };
    }

    function syncThemeState(oController, sSource, oThemeResult) {
        var oShellModel = ControllerModelRuntime.shell(oController);
        var oShellPatch = buildShellModelData(oThemeResult);
        var oModelPatch = {};

        oModelPatch[SHELL_PATHS.THEME_MODE] = oShellPatch.themeMode;
        oModelPatch[SHELL_PATHS.ANIMATION_ENABLED] = oShellPatch.animationEnabled;
        if (oShellModel) {
            ModelStateRuntime.setManyOnModel(oShellModel, oModelPatch);
        }
        DebugLogger.info("theme", "sync", {
            animationEnabled: oShellPatch.animationEnabled,
            appliedTheme: oThemeResult && oThemeResult.theme,
            source: sSource || SYNC_SOURCES.INIT,
            themeMode: oShellPatch.themeMode
        });
    }

    function initStateBoundUi(oController, oStateModel) {
        var oResolvedState;
        var iBootstrapRetryMs;

        if (oController._oRouteModeCoordinator) {
            return;
        }
        oResolvedState = oStateModel || resolveStateModel(oController);
        if (!oResolvedState) {
            SchedulingRuntime.restartTimer(0, function () {
                initStateBoundUi(oController);
            }, Number((TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50));
            return;
        }
        iBootstrapRetryMs = Number(TimeConfigService.read(oResolvedState, "bootstrapRetryMs") || (TimerDefaults.bootstrapRetryMs || {}).defaultValue || 50);
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
            var oState = resolveStateModel(oController);

            syncThemeState(oController, SYNC_SOURCES.INIT, oApplied);
            ensureControllerStateModel(oController, oState);
            if (oState) {
                ModelStateRuntime.setManyOnModel(oState, {
                    [ModelPathContracts.LAYOUT]: ModelStateRuntime.readOnModel(oState, ModelPathContracts.LAYOUT, LAYOUTS.ONE_COLUMN) || LAYOUTS.ONE_COLUMN,
                    [ModelPathContracts.SELECTED_ID]: typeof ModelStateRuntime.readOnModel(oState, ModelPathContracts.SELECTED_ID, undefined) === TYPE_UNDEFINED
                        ? null
                        : ModelStateRuntime.readOnModel(oState, ModelPathContracts.SELECTED_ID, null)
                });
            }
            initStateBoundUi(oController, oState);
        },
        onSetThemeMode: function (oController, sMode) {
            syncThemeState(oController, SYNC_SOURCES.SETTINGS, oController.setThemeMode(sMode));
        },
        onToggleTheme: function (oController, oClickXY) {
            syncThemeState(oController, SYNC_SOURCES.TOGGLE, oController.toggleTheme(oClickXY));
        },
        onToggleThemeAnimation: function (oController, bEnabled) {
            syncThemeState(oController, SYNC_SOURCES.ANIMATION, oController.setThemeAnimationEnabled(!!bEnabled));
        },
        onExit: function (oController) {
            if (oController._oRouteModeCoordinator) {
                oController._oRouteModeCoordinator.stop();
                oController._oRouteModeCoordinator = null;
            }
        }
    };
});
