sap.ui.define([], function () {
    "use strict";

    function ensureKpiBag(oStateModel) {
        if (!oStateModel || typeof oStateModel.getProperty !== "function" || typeof oStateModel.setProperty !== "function") {
            return null;
        }

        var oBag = oStateModel.getProperty("/operationalKpi");
        if (!oBag || typeof oBag !== "object") {
            oBag = {
                saveAttempts: 0,
                saveSuccess: 0,
                saveFailed: 0,
                saveLatencyMsLast: 0,
                saveLatencyMsAvg: 0,
                saveLatencySamples: 0,
                conflictCount: 0,
                validationFailures: 0,
                retryFailures: 0,
                retryLatencyMsLast: 0,
                retryLatencyMsAvg: 0,
                retryLatencySamples: 0
            };
            oStateModel.setProperty("/operationalKpi", oBag);
        }
        return oBag;
    }

    function beginLatencySample() {
        return Date.now();
    }

    function applyLatencySample(oBag, sPrefix, iStartedAt) {
        var iElapsed = Math.max(0, Date.now() - Number(iStartedAt || 0));
        var sLast = "/operationalKpi/" + sPrefix + "LatencyMsLast";
        var sAvg = "/operationalKpi/" + sPrefix + "LatencyMsAvg";
        var sSamples = "/operationalKpi/" + sPrefix + "LatencySamples";
        return {
            elapsed: iElapsed,
            pathLast: sLast,
            pathAvg: sAvg,
            pathSamples: sSamples
        };
    }

    function finishLatencySample(oStateModel, sPrefix, iStartedAt) {
        var oBag = ensureKpiBag(oStateModel);
        if (!oBag) {
            return { ok: false, reason: "missing_state_model_adapter" };
        }

        var oSample = applyLatencySample(oBag, sPrefix, iStartedAt);
        var iSamples = Number(oBag[sPrefix + "LatencySamples"] || 0) + 1;
        var nAvg = Number(oBag[sPrefix + "LatencyMsAvg"] || 0);
        var nNextAvg = ((nAvg * (iSamples - 1)) + oSample.elapsed) / iSamples;

        oStateModel.setProperty(oSample.pathLast, oSample.elapsed);
        oStateModel.setProperty(oSample.pathSamples, iSamples);
        oStateModel.setProperty(oSample.pathAvg, Number(nNextAvg.toFixed(2)));

        return { ok: true, elapsed: oSample.elapsed, samples: iSamples };
    }

    function increment(oStateModel, sPath) {
        var oBag = ensureKpiBag(oStateModel);
        if (!oBag) {
            return { ok: false, reason: "missing_state_model_adapter" };
        }

        var iNext = Number(oStateModel.getProperty(sPath) || 0) + 1;
        oStateModel.setProperty(sPath, iNext);
        return { ok: true, value: iNext };
    }

    function markSaveAttempt(oStateModel) {
        return increment(oStateModel, "/operationalKpi/saveAttempts");
    }

    function markSaveSuccess(oStateModel) {
        return increment(oStateModel, "/operationalKpi/saveSuccess");
    }

    function markSaveFailed(oStateModel) {
        return increment(oStateModel, "/operationalKpi/saveFailed");
    }

    function markConflict(oStateModel) {
        return increment(oStateModel, "/operationalKpi/conflictCount");
    }

    function markValidationFailure(oStateModel) {
        return increment(oStateModel, "/operationalKpi/validationFailures");
    }

    function markRetryFailure(oStateModel) {
        return increment(oStateModel, "/operationalKpi/retryFailures");
    }

    return {
        ensureKpiBag: ensureKpiBag,
        beginLatencySample: beginLatencySample,
        finishLatencySample: finishLatencySample,
        markSaveAttempt: markSaveAttempt,
        markSaveSuccess: markSaveSuccess,
        markSaveFailed: markSaveFailed,
        markConflict: markConflict,
        markValidationFailure: markValidationFailure,
        markRetryFailure: markRetryFailure
    };
});
