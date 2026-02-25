sap.ui.define([], function () {
    "use strict";

    function resolveExportEntityFromAction(mArgs) {
        var sDefaultEntity = (mArgs && mArgs.defaultEntity) || "screen";
        var oEvent = mArgs && mArgs.event;
        var fnResolver = mArgs && mArgs.resolveEntityFromMenuEvent;

        if (typeof fnResolver === "function" && oEvent) {
            return fnResolver(oEvent, sDefaultEntity) || sDefaultEntity;
        }

        if (mArgs && mArgs.source && typeof mArgs.source.data === "function") {
            return mArgs.source.data("entity") || sDefaultEntity;
        }

        return sDefaultEntity;
    }

    function buildExportFilename(sEntity, fnNow) {
        var sSafeEntity = sEntity || "screen";
        var iNow = typeof fnNow === "function" ? fnNow() : Date.now();
        return "checklist_" + sSafeEntity + "_" + iNow;
    }

    function runExportLifecycle(mArgs) {
        var fnRunExportFlow = mArgs.runExportFlow;
        var fnBuildExportPromise = mArgs.buildExportPromise;

        return fnRunExportFlow({
            runWithLoading: mArgs.runWithLoading,
            buildExportPromise: fnBuildExportPromise,
            onEmpty: mArgs.onEmpty,
            onSuccess: mArgs.onSuccess,
            onError: mArgs.onError
        });
    }

    return {
        resolveExportEntityFromAction: resolveExportEntityFromAction,
        buildExportFilename: buildExportFilename,
        runExportLifecycle: runExportLifecycle
    };
});
