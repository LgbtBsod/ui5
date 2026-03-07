sap.ui.define([
    "checklist/app/service/framework/Result",
    "checklist/app/service/domain/shared/usecases/ComponentStartupDiagnosticsUseCase"
], function (Result, ComponentStartupDiagnosticsUseCase) {
    "use strict";

    return {
        execute: function (input, ctx) {
            var fnSync = ComponentStartupDiagnosticsUseCase.createCapabilitySync({
                stateModel: ctx.stateModel,
                getBackendMode: ctx.getBackendMode
            });
            ComponentStartupDiagnosticsUseCase.wireMetadataEvents({
                mainServiceModel: ctx.mainServiceModel,
                stateModel: ctx.stateModel,
                syncCapability: fnSync,
                onMetadataFailed: input && input.onMetadataFailed
            });
            return Promise.resolve(Result.ok({ wired: true }, []));
        }
    };
});
