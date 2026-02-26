sap.ui.define([], function () {
    "use strict";

    function runExecuteSearchFlow(mDeps) {
        if (!mDeps || typeof mDeps.runWithLoading !== "function" || typeof mDeps.runSearch !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_dependency" });
        }

        return mDeps.runWithLoading(function () {
            return mDeps.runSearch().then(function (aFiltered) {
                if (typeof mDeps.applyRows === "function") {
                    mDeps.applyRows(aFiltered || []);
                }
                if (typeof mDeps.afterApply === "function") {
                    mDeps.afterApply();
                }
                return { ok: true, reason: "applied", count: (aFiltered || []).length };
            });
        });
    }

    return {
        runExecuteSearchFlow: runExecuteSearchFlow
    };
});
