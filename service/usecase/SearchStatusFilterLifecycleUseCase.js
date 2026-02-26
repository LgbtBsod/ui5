sap.ui.define([], function () {
    "use strict";

    function runStatusFilterLifecycle(mArgs) {
        var oEvent = mArgs && mArgs.event;
        var fnGetSource = mArgs && mArgs.getSource;
        var oSource = typeof fnGetSource === "function" ? fnGetSource(oEvent) : null;
        if (!oSource) {
            return { ok: false, reason: "missing_event_source" };
        }

        var fnReadData = mArgs && mArgs.readData;
        if (typeof fnReadData !== "function") {
            return { ok: false, reason: "missing_data_reader" };
        }

        var sFilterPath = fnReadData(oSource, "filterPath");
        var sFilterValue = fnReadData(oSource, "filterValue");
        if (!sFilterPath) {
            return { ok: false, reason: "missing_filter_path" };
        }

        var fnApplyStatusFilter = mArgs && mArgs.applyStatusFilter;
        if (typeof fnApplyStatusFilter !== "function") {
            return { ok: false, reason: "missing_apply_adapter" };
        }

        fnApplyStatusFilter(sFilterPath, sFilterValue);

        var fnAfterApply = mArgs && mArgs.afterApply;
        if (typeof fnAfterApply === "function") {
            fnAfterApply({ filterPath: sFilterPath, filterValue: sFilterValue });
        }

        return {
            ok: true,
            reason: "status_filter_applied",
            filterPath: sFilterPath,
            filterValue: sFilterValue || ""
        };
    }

    return {
        runStatusFilterLifecycle: runStatusFilterLifecycle
    };
});
