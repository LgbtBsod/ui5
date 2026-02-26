sap.ui.define([
    "sap_ui5/service/usecase/StartupCapabilityDiagnosticsUseCase"
], function (StartupCapabilityDiagnosticsUseCase) {
    "use strict";

    function createCapabilitySync(mDeps) {
        var oStateModel = mDeps && mDeps.stateModel;
        var fnGetBackendMode = mDeps && mDeps.getBackendMode;

        return function (mOverrides) {
            if (!oStateModel || typeof oStateModel.getProperty !== "function") {
                return { ok: false, reason: "missing_state_model_adapter" };
            }

            var oDiagnostics = StartupCapabilityDiagnosticsUseCase.buildDiagnostics(Object.assign({
                backendMode: typeof fnGetBackendMode === "function" ? fnGetBackendMode() : "fake",
                metadataOk: oStateModel.getProperty("/mainServiceMetadataOk"),
                metadataError: oStateModel.getProperty("/mainServiceMetadataError")
            }, mOverrides || {}));

            return StartupCapabilityDiagnosticsUseCase.applyToStateModel(oStateModel, oDiagnostics);
        };
    }

    function wireMetadataEvents(mDeps) {
        var oMainServiceModel = mDeps && mDeps.mainServiceModel;
        var oStateModel = mDeps && mDeps.stateModel;
        var fnSync = mDeps && mDeps.syncCapability;

        if (!oMainServiceModel || !oStateModel || typeof fnSync !== "function") {
            return { ok: false, reason: "missing_dependency" };
        }

        oStateModel.setProperty("/mainServiceMetadataOk", null);
        oStateModel.setProperty("/mainServiceMetadataError", "");
        fnSync({ metadataOk: null });

        oMainServiceModel.attachMetadataLoaded(function () {
            oStateModel.setProperty("/mainServiceMetadataOk", true);
            oStateModel.setProperty("/mainServiceMetadataError", "");
            fnSync();
        });

        oMainServiceModel.attachMetadataFailed(function (oEvent) {
            var oParams = oEvent && oEvent.getParameters ? oEvent.getParameters() : {};
            var sReason = (oParams && (oParams.message || oParams.responseText)) || "Metadata request failed";
            oStateModel.setProperty("/mainServiceMetadataOk", false);
            oStateModel.setProperty("/mainServiceMetadataError", sReason);
            fnSync();
        });

        return { ok: true };
    }

    return {
        createCapabilitySync: createCapabilitySync,
        wireMetadataEvents: wireMetadataEvents
    };
});
