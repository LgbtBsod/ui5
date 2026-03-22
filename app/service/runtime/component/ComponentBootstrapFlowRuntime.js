sap.ui.define([], function () {
    "use strict";

    function initializeStartupState(oComponent, UIComponent, InteractionFX, ThemeService, aInitArgs) {
        oComponent._oInteractionFX = InteractionFX;
        UIComponent.prototype.init.apply(oComponent, aInitArgs || []);
        oComponent._startupPerf = oComponent._startupPerf || {
            t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
            firstRouteReadyLogged: false,
            analyticsStartedLogged: false
        };
        ThemeService.syncDocumentRootClasses();
    }

    function createBootstrapContext(mStaticDeps, oDependencyBuilder, ComponentAppRuntime) {
        var mGroups = oDependencyBuilder.build(mStaticDeps);
        var mDeps = oDependencyBuilder.flatten(mGroups);

        mDeps.ComponentRuntimeSupport = ComponentAppRuntime.buildComponentRuntimeSupport();

        return {
            groups: mGroups,
            deps: mDeps
        };
    }

    function runDiagnostics(mDiagnosticsUseCase, mBackendModeContracts, mBootstrapDeps, mModelBootstrap) {
        return mDiagnosticsUseCase.execute({}, {
            mainServiceModel: mModelBootstrap.mainServiceModel,
            stateModel: mModelBootstrap.models.stateModel,
            getBackendMode: function () {
                return mBackendModeContracts.MODES.REAL;
            },
            onMetadataFailed: function () {
                mBootstrapDeps.ModelStateRuntime.writeOnModel(
                    mModelBootstrap.models.stateModel,
                    mBackendModeContracts.PATHS.BACKEND_MODE,
                    mBackendModeContracts.MODES.REAL
                );
            }
        });
    }

    return {
        createBootstrapContext: createBootstrapContext,
        initializeStartupState: initializeStartupState,
        runDiagnostics: runDiagnostics
    };
});
