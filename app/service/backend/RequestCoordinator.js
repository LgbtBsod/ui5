sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/CorrelationId",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/InFlightRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/ResponseGuard",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestResiliencePolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/PromiseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (CorrelationId, InFlightRegistry, ResponseGuard, RequestResiliencePolicy, PromiseRuntime, SchedulingRuntime) {
    "use strict";

    function attachCorrelationId(oError, sCorrelationId) {
        var oResolved = oError || {};
        if (!oResolved.correlationId) {
            oResolved.correlationId = sCorrelationId;
        }
        return oResolved;
    }

    function createTimeoutError(sCorrelationId, iTimeoutMs) {
        return {
            code: "REQUEST_TIMEOUT",
            message: "Request timed out",
            statusCode: 0,
            correlationId: sCorrelationId,
            timeoutMs: Number(iTimeoutMs || 0) || 0
        };
    }

    function createOutdatedError(sCorrelationId, sGuardKey) {
        return {
            code: "OUTDATED_RESPONSE",
            message: "Outdated response ignored",
            statusCode: 0,
            correlationId: sCorrelationId,
            responseGuardKey: String(sGuardKey || ""),
            ignored: true,
            silent: true
        };
    }

    function normalizeFactoryResult(vResult) {
        if (vResult && typeof vResult === "object" && vResult.promise) {
            return {
                promise: Promise.resolve(vResult.promise),
                abort: typeof vResult.abort === "function" ? vResult.abort : function () { return; }
            };
        }
        return {
            promise: Promise.resolve(vResult),
            abort: function () { return; }
        };
    }

    function withTimeout(oRequestHandle, iTimeoutMs, sCorrelationId) {
        var iTimer = 0;
        return PromiseRuntime.withFinally(new Promise(function (resolve, reject) {
            iTimer = SchedulingRuntime.restartTimer(iTimer, function () {
                try {
                    oRequestHandle.abort();
                } catch (_e) {
                    return;
                }
                reject(createTimeoutError(sCorrelationId, iTimeoutMs));
            }, iTimeoutMs);

            oRequestHandle.promise.then(resolve, reject);
        }), function () {
            SchedulingRuntime.clearTimer(iTimer);
        });
    }

    function waitBeforeRetry(iDelayMs) {
        var iResolvedDelay = Math.max(0, Number(iDelayMs) || 0);
        if (!iResolvedDelay) {
            return Promise.resolve();
        }
        return SchedulingRuntime.wait(iResolvedDelay);
    }

    function execute(mRequest) {
        var oRequest = mRequest || {};
        var sMethod = String(oRequest.method || "GET").trim().toUpperCase() || "GET";
        var sCorrelationId = String(oRequest.correlationId || CorrelationId.next("req")).trim();
        var sDedupeKey = RequestResiliencePolicy.isSafeRead(sMethod) ? String(oRequest.dedupeKey || "").trim() : "";
        var sGuardKey = String(oRequest.responseGuardKey || "").trim();
        var iTimeoutMs = RequestResiliencePolicy.resolveTimeoutMs(sMethod, oRequest.timeoutMs);
        var iRetryLimit = RequestResiliencePolicy.resolveRetryCount(sMethod, oRequest.retryCount);
        var oExisting = sDedupeKey ? InFlightRegistry.get(sDedupeKey) : null;
        var oStoredEntry = null;
        var iGuardToken = 0;

        function runAttempt(iAttempt, bCsrfRetried) {
            var oFactoryResult;
            var oSupersededHandle;
            try {
                oFactoryResult = normalizeFactoryResult(
                    typeof oRequest.requestFactory === "function"
                        ? oRequest.requestFactory({
                            attempt: iAttempt + 1,
                            correlationId: sCorrelationId
                        })
                        : Promise.resolve(null)
                );
            } catch (oError) {
                return Promise.reject(attachCorrelationId(oError, sCorrelationId));
            }
            if (sGuardKey && RequestResiliencePolicy.isSafeRead(sMethod)) {
                oSupersededHandle = ResponseGuard.replaceActiveHandle(sGuardKey, iGuardToken, oFactoryResult);
                if (oSupersededHandle && oSupersededHandle.handle && typeof oSupersededHandle.handle.abort === "function" &&
                    Number(oSupersededHandle.token || 0) !== Number(iGuardToken || 0)) {
                    try {
                        oSupersededHandle.handle.abort();
                    } catch (_abortError) {
                        // Abort is best-effort; outdated response guarding still applies.
                    }
                }
            }
            return withTimeout(oFactoryResult, iTimeoutMs, sCorrelationId).then(function (vResponse) {
                if (sGuardKey && !ResponseGuard.isCurrent(sGuardKey, iGuardToken)) {
                    throw createOutdatedError(sCorrelationId, sGuardKey);
                }
                return vResponse;
            }).catch(function (oError) {
                var oResolvedError = attachCorrelationId(oError, sCorrelationId);
                if (sGuardKey && !ResponseGuard.isCurrent(sGuardKey, iGuardToken)) {
                    throw createOutdatedError(sCorrelationId, sGuardKey);
                }
                var oPolicy = RequestResiliencePolicy.classify(sMethod, oResolvedError);
                if (!RequestResiliencePolicy.isSafeRead(sMethod) && !bCsrfRetried && oPolicy.kind === "CSRF" && typeof oRequest.csrfRefreshFactory === "function") {
                    return Promise.resolve(oRequest.csrfRefreshFactory({
                        attempt: iAttempt + 1,
                        correlationId: sCorrelationId,
                        error: oResolvedError
                    })).then(function () {
                        return runAttempt(iAttempt, true);
                    });
                }
                if (iAttempt < iRetryLimit && oPolicy.retryable) {
                    return waitBeforeRetry(RequestResiliencePolicy.resolveRetryDelayMs(
                        sMethod,
                        iAttempt,
                        oRequest.retryBaseDelayMs,
                        oRequest.retryMaxDelayMs
                    )).then(function () {
                        return runAttempt(iAttempt + 1, bCsrfRetried);
                    });
                }
                throw oResolvedError;
            });
        }

        if (oExisting && oExisting.promise) {
            return oExisting.promise;
        }

        if (sGuardKey) {
            iGuardToken = ResponseGuard.mark(sGuardKey);
        }

        var pFinal = PromiseRuntime.withFinally(runAttempt(0, false), function () {
            if (sGuardKey) {
                ResponseGuard.clearActiveHandle(sGuardKey, iGuardToken);
            }
            if (sDedupeKey) {
                InFlightRegistry.remove(sDedupeKey, oStoredEntry);
            }
        });

        if (sDedupeKey) {
            oStoredEntry = { promise: pFinal };
            InFlightRegistry.set(sDedupeKey, oStoredEntry);
        }

        return pFinal;
    }

    return {
        execute: execute
    };
});
