sap.ui.define([], function () {
    "use strict";

    function runCreateNavigationFlow(mArgs) {
        var fnConfirm = mArgs && mArgs.confirmNavigation;
        var fnBuildCreateIntent = mArgs && mArgs.buildCreateIntent;
        var fnApplyIntent = mArgs && mArgs.applyIntent;

        if (typeof fnApplyIntent !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_nav_adapter" });
        }

        return Promise.resolve(typeof fnConfirm === "function" ? fnConfirm() : true).then(function (bCanNavigate) {
            if (!bCanNavigate) {
                return { ok: false, reason: "cancelled" };
            }

            var mIntent = typeof fnBuildCreateIntent === "function" ? fnBuildCreateIntent() : null;
            if (!mIntent) {
                return { ok: false, reason: "null_intent" };
            }

            var bApplied = fnApplyIntent(mIntent);
            return bApplied
                ? { ok: true, reason: "applied", intent: mIntent }
                : { ok: false, reason: "missing_nav_adapter", intent: mIntent };
        }).catch(function (oError) {
            return { ok: false, reason: "confirm_error", error: oError };
        });
    }

    function runCopyNavigationFlow(mArgs) {
        var fnResolveSelectedId = mArgs && mArgs.resolveSelectedId;
        var fnConfirm = mArgs && mArgs.confirmNavigation;
        var fnBuildCopyIntent = mArgs && mArgs.buildCopyIntent;
        var fnApplyIntent = mArgs && mArgs.applyIntent;

        if (typeof fnApplyIntent !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_nav_adapter" });
        }

        var sSelectedId = typeof fnResolveSelectedId === "function" ? (fnResolveSelectedId() || "") : "";
        if (!sSelectedId) {
            return Promise.resolve({ ok: false, reason: "missing_selection" });
        }

        return Promise.resolve(typeof fnConfirm === "function" ? fnConfirm() : true).then(function (bCanNavigate) {
            if (!bCanNavigate) {
                return { ok: false, reason: "cancelled", selectedId: sSelectedId };
            }

            var mIntent = typeof fnBuildCopyIntent === "function" ? fnBuildCopyIntent(sSelectedId) : null;
            if (!mIntent) {
                return { ok: false, reason: "null_intent", selectedId: sSelectedId };
            }

            var bApplied = fnApplyIntent(mIntent);
            return bApplied
                ? { ok: true, reason: "applied", selectedId: sSelectedId, intent: mIntent }
                : { ok: false, reason: "missing_nav_adapter", selectedId: sSelectedId, intent: mIntent };
        }).catch(function (oError) {
            return { ok: false, reason: "confirm_error", selectedId: sSelectedId, error: oError };
        });
    }

    return {
        runCreateNavigationFlow: runCreateNavigationFlow,
        runCopyNavigationFlow: runCopyNavigationFlow
    };
});
