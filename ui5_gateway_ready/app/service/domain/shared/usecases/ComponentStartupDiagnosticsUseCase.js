sap.ui.define([
    "checklist/app/service/domain/shared/usecases/StartupCapabilityDiagnosticsUseCase",
    "checklist/app/service/framework/Result"
], function (StartupCapabilityDiagnosticsUseCase, Result) {
    "use strict";

    function createCapabilitySync(mDeps) {
        var oStateModel = mDeps && mDeps.stateModel;
        var fnGetBackendMode = mDeps && mDeps.getBackendMode;
        return function (mOverrides) {
            if (!oStateModel || typeof oStateModel.getProperty !== "function") return { ok: false, reason: "missing_state_model_adapter" };
            var oDiagnostics = StartupCapabilityDiagnosticsUseCase.buildDiagnostics(Object.assign({ backendMode: typeof fnGetBackendMode === "function" ? fnGetBackendMode() : "real", metadataOk: oStateModel.getProperty("/mainServiceMetadataOk"), metadataError: oStateModel.getProperty("/mainServiceMetadataError") }, mOverrides || {}));
            return StartupCapabilityDiagnosticsUseCase.applyToStateModel(oStateModel, oDiagnostics);
        };
    }

    function wireMetadataEvents(mDeps) {
        var oMainServiceModel = mDeps && mDeps.mainServiceModel;
        var oStateModel = mDeps && mDeps.stateModel;
        var fnSync = mDeps && mDeps.syncCapability;
        if (!oMainServiceModel || !oStateModel || typeof fnSync !== "function") return { ok: false, reason: "missing_dependency" };
        oStateModel.setProperty("/mainServiceMetadataOk", null);
        oStateModel.setProperty("/mainServiceMetadataError", "");
        fnSync({ metadataOk: null });
        oMainServiceModel.attachMetadataLoaded(function () { oStateModel.setProperty("/mainServiceMetadataOk", true); oStateModel.setProperty("/mainServiceMetadataError", ""); fnSync(); });
        oMainServiceModel.attachMetadataFailed(function (oEvent) {
            var oParams = oEvent && oEvent.getParameters ? oEvent.getParameters() : {};
            var sReason = (oParams && (oParams.message || oParams.responseText)) || "Metadata request failed";
            oStateModel.setProperty("/mainServiceMetadataOk", false);
            oStateModel.setProperty("/mainServiceMetadataError", sReason);
            fnSync();
            if (typeof mDeps.onMetadataFailed === "function") mDeps.onMetadataFailed(sReason);
        });
        return { ok: true };
    }

    return { execute: function (input, ctx) {
        var fnSync = createCapabilitySync({ stateModel: ctx.stateModel, getBackendMode: ctx.getBackendMode });
        var oWire = wireMetadataEvents({ mainServiceModel: ctx.mainServiceModel, stateModel: ctx.stateModel, syncCapability: fnSync, onMetadataFailed: input && input.onMetadataFailed });
        return oWire && oWire.ok ? Result.ok(oWire, []) : Result.fail(oWire, []);
    }, createCapabilitySync: createCapabilitySync, wireMetadataEvents: wireMetadataEvents };
});
