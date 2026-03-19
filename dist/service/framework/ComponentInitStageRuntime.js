sap.ui.define([], function () {
    "use strict";

    function createModelStage(oComponent, mDeps) {
        var sMainServiceUri = oComponent.getManifestEntry("/sap.app/dataSources/mainService/uri");
        var mModels = mDeps.ComponentModelInitRuntime.initializeModels(oComponent, mDeps);

        if (!sMainServiceUri) {
            throw new Error("Manifest-driven mainService dataSource is missing. Check sap.app/dataSources/mainService/uri in manifest.json.");
        }

        return {
            models: mModels,
            mainServiceModel: mDeps.ComponentMainServiceRuntime.createMainServiceModel(oComponent, mDeps, sMainServiceUri)
        };
    }

    function createCoreStage(oComponent, mDeps, mModels, mHelpers) {
        var oStateModel = mModels.stateModel;
        var oCoreRuntime = mDeps.ComponentCoreInitRuntime.initializeComponentRuntime(oComponent, mDeps, mModels, {
            buildActionValidators: mHelpers.buildActionValidators,
            createApplyFacadeResult: mHelpers.createApplyFacadeResult
        });
        return {
            coreRuntime: oCoreRuntime,
            emitTelemetry: function (sEventName, oPayload) {
                return mDeps.WorkflowTelemetry.emit(sEventName, {
                    stateModel: oStateModel,
                    payload: oPayload || {}
                });
            },
            timerDefaults: mDeps.ComponentStateSeedRuntime.seedInitialState(oStateModel, mDeps.StatePaths, mDeps.TimeConfigService)
        };
    }

    return {
        createCoreStage: createCoreStage,
        createModelStage: createModelStage
    };
});
