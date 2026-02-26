sap.ui.define([], function () {
    "use strict";

    function runSelectionMetaSync(mDeps) {
        if (!mDeps || typeof mDeps.syncDirty !== "function" || typeof mDeps.updateSelectionState !== "function" || typeof mDeps.updateDerivedRootResult !== "function") {
            return { ok: false, reason: "missing_dependency" };
        }

        mDeps.syncDirty();
        mDeps.updateSelectionState();
        mDeps.updateDerivedRootResult();

        return { ok: true, reason: "selection_meta_synced" };
    }

    return {
        runSelectionMetaSync: runSelectionMetaSync
    };
});
