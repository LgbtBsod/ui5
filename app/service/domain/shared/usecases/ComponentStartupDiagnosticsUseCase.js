sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/StartupCapabilityDiagnosticsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/BackendModeContracts"
], function (StartupCapabilityDiagnosticsUseCase, Result, BackendModeContracts) {
    "use strict";

    function createCapabilitySync(mDeps) {
        var oStateModel = mDeps && mDeps.stateModel;
        var fnGetBackendMode = mDeps && mDeps.getBackendMode;
        return function (mOverrides) {
            if (!oStateModel || typeof oStateModel.getProperty !== "function") return { ok: false, reason: "missing_state_model_adapter" };
            var oDiagnostics = StartupCapabilityDiagnosticsUseCase.buildDiagnostics(Object.assign({ backendMode: typeof fnGetBackendMode === "function" ? fnGetBackendMode() : BackendModeContracts.MODES.REAL, metadataOk: oStateModel.getProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_OK), metadataError: oStateModel.getProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_ERROR) }, mOverrides || {}));
            return StartupCapabilityDiagnosticsUseCase.applyToStateModel(oStateModel, oDiagnostics);
        };
    }

    function wireMetadataEvents(mDeps) {
        var oMainServiceModel = mDeps && mDeps.mainServiceModel;
        var oStateModel = mDeps && mDeps.stateModel;
        var fnSync = mDeps && mDeps.syncCapability;
        if (!oMainServiceModel || !oStateModel || typeof fnSync !== "function") return { ok: false, reason: "missing_dependency" };
        oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_OK, null);
        oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_ERROR, "");
        fnSync({ metadataOk: null });
        oMainServiceModel.attachMetadataLoaded(function () { oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_OK, true); oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_ERROR, ""); fnSync(); });
        oMainServiceModel.attachMetadataFailed(function (oEvent) {
            var oParams = oEvent && oEvent.getParameters ? oEvent.getParameters() : {};
            var sReason = (oParams && (oParams.message || oParams.responseText)) || "Metadata request failed";
            oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_OK, false);
            oStateModel.setProperty(BackendModeContracts.PATHS.MAIN_SERVICE_METADATA_ERROR, sReason);
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
