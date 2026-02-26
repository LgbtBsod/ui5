sap.ui.define([], function () {
    "use strict";

    function resolveModeLabel(sMode) {
        if (sMode === "real") {
            return "real";
        }
        return "fake";
    }

    function buildDiagnostics(mInput) {
        var m = mInput || {};
        var sMode = resolveModeLabel(m.backendMode);
        var bMetadataOk = m.metadataOk === true;
        var bMetadataFailed = m.metadataOk === false;

        var sDegradedReason = "";
        if (sMode !== "real") {
            sDegradedReason = "backend_fake_mode";
        } else if (bMetadataFailed) {
            sDegradedReason = "metadata_unavailable";
        }

        var sStatus = sDegradedReason ? "degraded" : (bMetadataOk ? "ready" : "pending");
        var sMessageKey = "capabilityReady";
        if (sStatus === "pending") {
            sMessageKey = "capabilityPending";
        } else if (sDegradedReason === "backend_fake_mode") {
            sMessageKey = "capabilityDegradedFakeMode";
        } else if (sDegradedReason === "metadata_unavailable") {
            sMessageKey = "capabilityDegradedMetadata";
        }

        return {
            status: sStatus,
            degradedReason: sDegradedReason,
            messageKey: sMessageKey,
            backend: {
                mode: sMode,
                configuredMode: m.backendMode || "fake"
            },
            metadata: {
                ok: bMetadataOk,
                error: m.metadataError || ""
            },
            checkedAt: new Date().toISOString()
        };
    }

    function applyToStateModel(oStateModel, oDiagnostics) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return { ok: false, reason: "missing_state_model_adapter" };
        }
        oStateModel.setProperty("/capabilityDiagnostics", oDiagnostics || {});
        oStateModel.setProperty("/capabilityStatus", (oDiagnostics && oDiagnostics.status) || "pending");
        oStateModel.setProperty("/capabilityDegradedReason", (oDiagnostics && oDiagnostics.degradedReason) || "");
        oStateModel.setProperty("/capabilityMessageKey", (oDiagnostics && oDiagnostics.messageKey) || "capabilityPending");
        return { ok: true };
    }

    return {
        buildDiagnostics: buildDiagnostics,
        applyToStateModel: applyToStateModel
    };
});
