sap.ui.define([
    "sap_ui5/service/usecase/DetailSaveConflictFlowUseCase"
], function (DetailSaveConflictFlowUseCase) {
    "use strict";

    function createConflictAdapter(mArgs) {
        return DetailSaveConflictFlowUseCase.buildConflictHandler({
            reloadLabel: mArgs.reloadLabel,
            overwriteLabel: mArgs.overwriteLabel,
            onReload: mArgs.onReload,
            onOverwrite: mArgs.onOverwrite
        });
    }

    function createBackendErrorAdapter(mArgs) {
        return {
            onConflictChoice: createConflictAdapter(mArgs)
        };
    }


    function isErrorLike(vResult) {
        return !!(vResult && typeof vResult === "object" && typeof vResult.message === "string"
            && (typeof vResult.name === "string" || typeof vResult.stack === "string"));
    }

    function normalizeHandledResult(vResult, mArgs) {
        if (vResult && typeof vResult === "object" && vResult.reason) {
            return {
                ok: vResult.ok !== false,
                reason: vResult.reason,
                result: Object.prototype.hasOwnProperty.call(vResult, "result") ? vResult.result : vResult
            };
        }

        if (isErrorLike(vResult)) {
            return {
                ok: false,
                reason: "backend_error",
                result: null,
                error: vResult
            };
        }

        if (vResult === null || typeof vResult === "undefined") {
            return { ok: false, reason: "cancelled", result: null };
        }

        if (vResult === mArgs.reloadLabel) {
            return { ok: true, reason: "legacy_reload", result: vResult };
        }

        if (vResult === mArgs.overwriteLabel) {
            return { ok: true, reason: "legacy_overwrite", result: vResult };
        }

        return { ok: true, reason: "handled", result: vResult };
    }

    function handleSaveError(mArgs) {
        var mAdapter = createBackendErrorAdapter(mArgs);
        return mArgs.handleBackendError(mArgs.host, mArgs.error, mAdapter).then(function (vResult) {
            return normalizeHandledResult(vResult, mArgs);
        }).catch(function (oError) {
            return {
                ok: false,
                reason: "backend_error",
                result: null,
                error: oError || null
            };
        });
    }

    return {
        createConflictAdapter: createConflictAdapter,
        createBackendErrorAdapter: createBackendErrorAdapter,
        normalizeHandledResult: normalizeHandledResult,
        handleSaveError: handleSaveError
    };
});
