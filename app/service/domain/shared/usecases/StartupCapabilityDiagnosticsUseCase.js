sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/BackendModeContracts"
], function (Result, BackendModeContracts) {
    "use strict";

    function buildDiagnostics(mInput) {
        var m = mInput || {};
        var sMode = String(m.backendMode || BackendModeContracts.MODES.REAL);
        var bMetadataOk = m.metadataOk === true;
        var bMetadataFailed = m.metadataOk === false;
        var sReason = bMetadataFailed ? BackendModeContracts.CAPABILITY.METADATA_UNAVAILABLE : "";
        var sStatus = sReason ? BackendModeContracts.CAPABILITY.DEGRADED : (bMetadataOk ? BackendModeContracts.CAPABILITY.READY : BackendModeContracts.CAPABILITY.PENDING);
        var sKey = sStatus === BackendModeContracts.CAPABILITY.PENDING ? "capabilityPending" : (sReason ? "capabilityDegradedMetadata" : "capabilityReady");
        return { status: sStatus, degradedReason: sReason, messageKey: sKey, backend: { mode: sMode, configuredMode: BackendModeContracts.MODES.REAL }, metadata: { ok: bMetadataOk, error: m.metadataError || "" }, checkedAt: new Date().toISOString() };
    }

    function applyToStateModel(oStateModel, oDiagnostics) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") return { ok: false, reason: "missing_state_model_adapter" };
        oStateModel.setProperty("/capabilityDiagnostics", oDiagnostics || {});
        oStateModel.setProperty("/capabilityStatus", (oDiagnostics && oDiagnostics.status) || BackendModeContracts.CAPABILITY.PENDING);
        oStateModel.setProperty("/capabilityDegradedReason", (oDiagnostics && oDiagnostics.degradedReason) || "");
        oStateModel.setProperty("/capabilityMessageKey", (oDiagnostics && oDiagnostics.messageKey) || "capabilityPending");
        return { ok: true };
    }

    return { execute: function (input) {
        var oDiag = buildDiagnostics(input || {});
        return Result.ok(oDiag, []);
    }, buildDiagnostics: buildDiagnostics, applyToStateModel: applyToStateModel };
});
