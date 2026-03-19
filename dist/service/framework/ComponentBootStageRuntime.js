sap.ui.define([], function () {
    "use strict";

    function allSettledPolyfill(aPromises) {
        return Promise.all((aPromises || []).map(function (p) {
            return Promise.resolve(p).then(
                function (v) { return { status: "fulfilled", value: v }; },
                function (e) { return { status: "rejected", reason: e }; }
            );
        }));
    }

    function toStageError(vError, sFallbackMessage) {
        if (vError instanceof Error) {
            return vError;
        }
        return new Error(String(vError || sFallbackMessage || "boot_stage_failed"));
    }

    function resolveSettledStageError(oSettledResult, sFallbackMessage) {
        if (!oSettledResult) {
            return toStageError(null, sFallbackMessage);
        }
        if (oSettledResult.status === "rejected") {
            return toStageError(oSettledResult.reason, sFallbackMessage);
        }
        if (oSettledResult.value && oSettledResult.value.ok === false) {
            return toStageError(oSettledResult.value.error && oSettledResult.value.error.message, sFallbackMessage);
        }
        return null;
    }

    return {
        allSettledPolyfill: allSettledPolyfill,
        toStageError: toStageError,
        resolveSettledStageError: resolveSettledStageError
    };
});
