sap.ui.define([], function () {
    "use strict";

    function runRetryLifecycle(mArgs) {
        var fnBeginLatency = mArgs && mArgs.beginLatency;
        var iStartedAt = typeof fnBeginLatency === "function" ? fnBeginLatency() : 0;

        var fnRunRetryFlow = mArgs && mArgs.runRetryFlow;
        if (typeof fnRunRetryFlow !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_retry_flow" });
        }

        return fnRunRetryFlow().then(function (oResult) {
            var fnPresentRetryOutcome = mArgs && mArgs.presentRetryOutcome;
            var oPresentation = typeof fnPresentRetryOutcome === "function"
                ? fnPresentRetryOutcome(oResult)
                : { ok: true, reason: "presentation_skipped" };

            if (!oResult || oResult.ok !== true || (oPresentation && oPresentation.ok === false)) {
                var fnMarkRetryFailure = mArgs && mArgs.markRetryFailure;
                if (typeof fnMarkRetryFailure === "function") {
                    fnMarkRetryFailure();
                }
            }

            var fnFinishLatency = mArgs && mArgs.finishLatency;
            if (typeof fnFinishLatency === "function") {
                fnFinishLatency("retry", iStartedAt);
            }

            var fnAfterRetryApplied = mArgs && mArgs.afterRetryApplied;
            if (typeof fnAfterRetryApplied === "function") {
                fnAfterRetryApplied(oResult, oPresentation);
            }

            return {
                ok: !!(oResult && oResult.ok === true),
                reason: (oResult && oResult.reason) || "retry_completed",
                result: oResult,
                presentation: oPresentation
            };
        });
    }

    return {
        runRetryLifecycle: runRetryLifecycle
    };
});
