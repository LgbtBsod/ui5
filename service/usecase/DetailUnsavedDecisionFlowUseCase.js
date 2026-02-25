sap.ui.define([], function () {
    "use strict";

    function shouldProceedAfterDecision(sDecision) {
        return sDecision === "DISCARD" || sDecision === "SAVE";
    }

    function requestUnsavedDecision(mArgs) {
        var fnConfirm = mArgs && mArgs.confirmUnsavedAndHandle;
        if (typeof fnConfirm !== "function") {
            return Promise.resolve("CANCEL");
        }
        var oHost = mArgs && mArgs.host;
        var fnOnSave = mArgs && mArgs.onSave;

        return Promise.resolve(fnConfirm(oHost, fnOnSave)).catch(function () {
            return "CANCEL";
        });
    }

    function buildConfirmUnsavedAction(mArgs) {
        return function () {
            return requestUnsavedDecision(mArgs);
        };
    }

    function runUnsavedCloseFlow(mArgs) {
        if (!(mArgs && mArgs.isDirty)) {
            if (typeof (mArgs && mArgs.proceed) === "function") {
                mArgs.proceed();
            }
            return Promise.resolve({ proceeded: true, decision: "CLEAN" });
        }

        return requestUnsavedDecision(mArgs).then(function (sDecision) {
            var bProceed = shouldProceedAfterDecision(sDecision);
            if (bProceed && typeof (mArgs && mArgs.proceed) === "function") {
                mArgs.proceed();
            }
            return {
                proceeded: bProceed,
                decision: sDecision
            };
        });
    }

    return {
        shouldProceedAfterDecision: shouldProceedAfterDecision,
        requestUnsavedDecision: requestUnsavedDecision,
        buildConfirmUnsavedAction: buildConfirmUnsavedAction,
        runUnsavedCloseFlow: runUnsavedCloseFlow
    };
});
