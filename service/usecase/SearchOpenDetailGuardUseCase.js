sap.ui.define([], function () {
    "use strict";

    function normalizeId(vId) {
        return String(vId || "").trim();
    }

    function runOpenDetailFlow(mArgs) {
        var sId = normalizeId(mArgs && mArgs.id);
        if (!sId) {
            return Promise.resolve({ ok: false, reason: "missing_id" });
        }

        var fnConfirm = mArgs && mArgs.confirmNavigation;
        var pConfirm = typeof fnConfirm === "function"
            ? Promise.resolve(fnConfirm())
            : Promise.resolve(true);

        return pConfirm.then(function (bCanNavigate) {
            if (!bCanNavigate) {
                return { ok: false, reason: "cancelled", id: sId };
            }

            var fnBuildIntent = mArgs && mArgs.buildIntent;
            var mIntent = typeof fnBuildIntent === "function"
                ? fnBuildIntent(sId)
                : null;
            if (!mIntent) {
                return { ok: false, reason: "null_intent", id: sId };
            }

            var fnApplyIntent = mArgs && mArgs.applyIntent;
            if (typeof fnApplyIntent !== "function") {
                return { ok: false, reason: "missing_router_adapter", id: sId, intent: mIntent };
            }

            var bApplied = fnApplyIntent(mIntent);
            if (!bApplied) {
                return { ok: false, reason: "missing_router_adapter", id: sId, intent: mIntent };
            }

            return { ok: true, reason: "applied", id: sId, intent: mIntent };
        }).catch(function (oError) {
            return { ok: false, reason: "confirm_error", id: sId, error: oError };
        });
    }

    return {
        normalizeId: normalizeId,
        runOpenDetailFlow: runOpenDetailFlow
    };
});
