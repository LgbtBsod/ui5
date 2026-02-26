sap.ui.define([], function () {
    "use strict";

    function shouldReloadChoice(sChoice, sReloadLabel) {
        return sChoice === sReloadLabel;
    }

    function shouldOverwriteChoice(sChoice, sOverwriteLabel) {
        return sChoice === sOverwriteLabel;
    }

    function handleConflictChoice(sChoice, mOptions) {
        var sReloadLabel = mOptions.reloadLabel;
        var sOverwriteLabel = mOptions.overwriteLabel;

        if (shouldReloadChoice(sChoice, sReloadLabel)) {
            if (typeof mOptions.onReload !== "function") {
                return Promise.resolve({ ok: false, reason: "missing_reload_handler" });
            }
            return Promise.resolve(mOptions.onReload()).then(function (vResult) {
                return { ok: true, reason: "reloaded", result: vResult };
            });
        }
        if (shouldOverwriteChoice(sChoice, sOverwriteLabel)) {
            if (typeof mOptions.onOverwrite !== "function") {
                return Promise.resolve({ ok: false, reason: "missing_overwrite_handler" });
            }
            return Promise.resolve(mOptions.onOverwrite()).then(function (vResult) {
                return { ok: true, reason: "overwritten", result: vResult };
            });
        }
        return Promise.resolve({ ok: false, reason: "cancelled" });
    }

    return {
        shouldReloadChoice: shouldReloadChoice,
        shouldOverwriteChoice: shouldOverwriteChoice,
        handleConflictChoice: handleConflictChoice
    };
});
