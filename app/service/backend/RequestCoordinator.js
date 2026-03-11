sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/CorrelationId",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/InFlightRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/ResponseGuard",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestResiliencePolicy"
], function (CorrelationId, InFlightRegistry, ResponseGuard, RequestResiliencePolicy) {
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
        return new Promise(function (resolve, reject) {
            iTimer = window.setTimeout(function () {
                try {
                    oRequestHandle.abort();
                } catch (_e) {
                    return;
                }
                reject(createTimeoutError(sCorrelationId, iTimeoutMs));
            }, iTimeoutMs);

            oRequestHandle.promise.then(resolve, reject);
        }).finally(function () {
            window.clearTimeout(iTimer);
        });
    }

    function execute(mRequest) {
        var oRequest = mRequest || {};
        var sMethod = String(oRequest.method || "GET").trim().toUpperCase() || "GET";
        var sCorrelationId = String(oRequest.correlationId || CorrelationId.next("req")).trim();
        var sDedupeKey = RequestResiliencePolicy.isSafeRead(sMethod) ? String(oRequest.dedupeKey || "").trim() : "";
        var sGuardKey = String(oRequest.responseGuardKey || "").trim();
        var iTimeoutMs = RequestResiliencePolicy.resolveTimeoutMs(sMethod, oRequest.timeoutMs);
        var iRetryLimit = RequestResiliencePolicy.resolveRetryCount(sMethod, oRequest.retryCount);
        var iGuardToken = sGuardKey ? ResponseGuard.mark(sGuardKey) : 0;
        var oExisting = sDedupeKey ? InFlightRegistry.get(sDedupeKey) : null;
        var oStoredEntry = null;

        function runAttempt(iAttempt) {
            var oFactoryResult;
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
            return withTimeout(oFactoryResult, iTimeoutMs, sCorrelationId).then(function (vResponse) {
                if (sGuardKey && !ResponseGuard.isCurrent(sGuardKey, iGuardToken)) {
                    throw createOutdatedError(sCorrelationId, sGuardKey);
                }
                return vResponse;
            }).catch(function (oError) {
                var oResolvedError = attachCorrelationId(oError, sCorrelationId);
                var oPolicy = RequestResiliencePolicy.classify(sMethod, oResolvedError);
                if (iAttempt < iRetryLimit && oPolicy.retryable) {
                    return runAttempt(iAttempt + 1);
                }
                throw oResolvedError;
            });
        }

        if (oExisting && oExisting.promise) {
            return oExisting.promise;
        }

        var pFinal = runAttempt(0).finally(function () {
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
