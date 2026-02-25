sap.ui.define([], function () {
    "use strict";

    function normalizeEntity(sEntity, sDefaultEntity, aAllowedEntities) {
        var sResolved = String(sEntity || "").trim();
        var sFallback = String(sDefaultEntity || "screen").trim() || "screen";
        var aAllowed = Array.isArray(aAllowedEntities) && aAllowedEntities.length
            ? aAllowedEntities
            : ["screen", "barrier", "check"];

        if (!sResolved || aAllowed.indexOf(sResolved) < 0) {
            return sFallback;
        }
        return sResolved;
    }

    function resolveEntityFromIntent(mArgs) {
        var sDefaultEntity = (mArgs && mArgs.defaultEntity) || "screen";
        var aAllowed = mArgs && mArgs.allowedEntities;

        var sCandidate = "";
        if (typeof (mArgs && mArgs.resolveEntityFromMenuEvent) === "function" && mArgs && mArgs.event) {
            sCandidate = mArgs.resolveEntityFromMenuEvent(mArgs.event, sDefaultEntity) || "";
        } else if (mArgs && mArgs.source && typeof mArgs.source.data === "function") {
            sCandidate = mArgs.source.data("entity") || "";
        }

        var sEntity = normalizeEntity(sCandidate, sDefaultEntity, aAllowed);
        return {
            entity: sEntity,
            fallbackUsed: !sCandidate || sCandidate !== sEntity
        };
    }

    function runExportIntent(mArgs) {
        var oIntent = resolveEntityFromIntent(mArgs || {});
        var fnRunExport = mArgs && mArgs.runExport;
        if (typeof fnRunExport !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_run_export", intent: oIntent });
        }

        var fnIsEnabled = mArgs && mArgs.isEnabled;
        if (typeof fnIsEnabled === "function" && !fnIsEnabled()) {
            return Promise.resolve({ ok: false, reason: "disabled", intent: oIntent });
        }

        return Promise.resolve(fnRunExport(oIntent.entity)).then(function () {
            return { ok: true, reason: "applied", intent: oIntent };
        }).catch(function (oError) {
            return { ok: false, reason: "run_error", error: oError, intent: oIntent };
        });
    }

    return {
        normalizeEntity: normalizeEntity,
        resolveEntityFromIntent: resolveEntityFromIntent,
        runExportIntent: runExportIntent
    };
});
