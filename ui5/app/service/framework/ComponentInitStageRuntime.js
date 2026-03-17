sap.ui.define([], function () {
    "use strict";

    function createModelStage(oComponent, mDeps) {
        var sMainServiceUri = oComponent.getManifestEntry("/sap.app/dataSources/mainService/uri") || "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/";
        var mModels = mDeps.ComponentModelInitRuntime.initializeModels(oComponent, mDeps);
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
