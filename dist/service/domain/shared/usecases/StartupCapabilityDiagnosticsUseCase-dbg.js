sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"], function (Result) {
    "use strict";

    function buildDiagnostics(mInput) {
        var m = mInput || {};
        var sMode = "real";
        var bMetadataOk = m.metadataOk === true;
        var bMetadataFailed = m.metadataOk === false;
        var sReason = bMetadataFailed ? "metadata_unavailable" : "";
        var sStatus = sReason ? "degraded" : (bMetadataOk ? "ready" : "pending");
        var sKey = sStatus === "pending" ? "capabilityPending" : (sReason ? "capabilityDegradedMetadata" : "capabilityReady");
        return { status: sStatus, degradedReason: sReason, messageKey: sKey, backend: { mode: sMode, configuredMode: "real" }, metadata: { ok: bMetadataOk, error: m.metadataError || "" }, checkedAt: new Date().toISOString() };
    }

    function applyToStateModel(oStateModel, oDiagnostics) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") return { ok: false, reason: "missing_state_model_adapter" };
        oStateModel.setProperty("/capabilityDiagnostics", oDiagnostics || {});
        oStateModel.setProperty("/capabilityStatus", (oDiagnostics && oDiagnostics.status) || "pending");
        oStateModel.setProperty("/capabilityDegradedReason", (oDiagnostics && oDiagnostics.degradedReason) || "");
        oStateModel.setProperty("/capabilityMessageKey", (oDiagnostics && oDiagnostics.messageKey) || "capabilityPending");
        return { ok: true };
    }

    return { execute: function (input) {
        var oDiag = buildDiagnostics(input || {});
        return Result.ok(oDiag, []);
    }, buildDiagnostics: buildDiagnostics, applyToStateModel: applyToStateModel };
});
