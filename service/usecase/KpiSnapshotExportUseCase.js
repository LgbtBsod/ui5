sap.ui.define([], function () {
    "use strict";

    function toNumber(v) {
        var n = Number(v);
        return Number.isFinite(n) ? n : 0;
    }

    function clone(obj) {
        return JSON.parse(JSON.stringify(obj || {}));
    }

    function buildSnapshot(oStateModel, mOptions) {
        if (!oStateModel || typeof oStateModel.getProperty !== "function") {
            return { ok: false, reason: "missing_state_model_adapter" };
        }

        var oKpi = clone(oStateModel.getProperty("/operationalKpi"));
        var oSnapshot = {
            capturedAt: new Date().toISOString(),
            source: (mOptions && mOptions.source) || "runtime",
            mode: oStateModel.getProperty("/mode") || "READ",
            activeObjectId: oStateModel.getProperty("/activeObjectId") || null,
            metrics: {
                saveAttempts: toNumber(oKpi.saveAttempts),
                saveSuccess: toNumber(oKpi.saveSuccess),
                saveFailed: toNumber(oKpi.saveFailed),
                saveLatencyMsLast: toNumber(oKpi.saveLatencyMsLast),
                saveLatencyMsAvg: toNumber(oKpi.saveLatencyMsAvg),
                conflictCount: toNumber(oKpi.conflictCount),
                validationFailures: toNumber(oKpi.validationFailures),
                retryFailures: toNumber(oKpi.retryFailures),
                retryLatencyMsLast: toNumber(oKpi.retryLatencyMsLast),
                retryLatencyMsAvg: toNumber(oKpi.retryLatencyMsAvg)
            }
        };

        return { ok: true, snapshot: oSnapshot };
    }

    function appendSnapshot(oStateModel, oSnapshot) {
        if (!oStateModel || typeof oStateModel.getProperty !== "function" || typeof oStateModel.setProperty !== "function") {
            return { ok: false, reason: "missing_state_model_adapter" };
        }

        var aExisting = oStateModel.getProperty("/operationalKpiSnapshots");
        var aSnapshots = Array.isArray(aExisting) ? aExisting.slice() : [];
        aSnapshots.push(clone(oSnapshot));

        var iMax = toNumber(oStateModel.getProperty("/operationalKpiSnapshotLimit")) || 50;
        if (aSnapshots.length > iMax) {
            aSnapshots = aSnapshots.slice(aSnapshots.length - iMax);
        }

        oStateModel.setProperty("/operationalKpiSnapshots", aSnapshots);
        return { ok: true, count: aSnapshots.length };
    }

    function exportSnapshots(aSnapshots) {
        var aSafe = Array.isArray(aSnapshots) ? aSnapshots : [];
        return {
            exportedAt: new Date().toISOString(),
            total: aSafe.length,
            snapshots: clone(aSafe)
        };
    }

    return {
        buildSnapshot: buildSnapshot,
        appendSnapshot: appendSnapshot,
        exportSnapshots: exportSnapshots
    };
});
