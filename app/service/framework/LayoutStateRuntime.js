sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (ModelStateRuntime, StatePaths) {
    "use strict";

    function normalizeLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        if (sLayout === "MidColumnFullScreen") {
            return "MidColumnFullScreen";
        }
        if (sLayout === "TwoColumnsMidExpanded" || sLayout === "TwoColumnsBeginExpanded") {
            return "TwoColumnsMidExpanded";
        }
        return "OneColumn";
    }

    function toLayoutKind(vLayout) {
        var sLayout = normalizeLayout(vLayout);
        if (sLayout === "MidColumnFullScreen") {
            return "detailOnly";
        }
        if (sLayout === "OneColumn") {
            return "single";
        }
        return "split";
    }

    function normalizeMode(vMode, sFallback) {
        return String(vMode || sFallback || "READ").toUpperCase();
    }

    function normalizeState(vValue, sFallback) {
        return String(vValue || sFallback || "").toUpperCase();
    }

    function readLayout(oStateModel, sFallback) {
        return normalizeLayout(
            ModelStateRuntime.readOnModel(oStateModel, "/layout", sFallback || "OneColumn")
        );
    }

    function readMode(oStateModel, sFallback) {
        return normalizeMode(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""),
            sFallback || "READ"
        );
    }

    function readLockState(oStateModel, sFallback) {
        return normalizeState(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, ""),
            sFallback || "READ_ONLY"
        );
    }

    function readAutosaveState(oStateModel, sFallback) {
        return normalizeState(
            ModelStateRuntime.readOnModel(oStateModel, "/autosaveState", ""),
            sFallback || "IDLE"
        );
    }

    return {
        normalizeLayout: normalizeLayout,
        toLayoutKind: toLayoutKind,
        normalizeMode: normalizeMode,
        normalizeState: normalizeState,
        readLayout: readLayout,
        readMode: readMode,
        readLockState: readLockState,
        readAutosaveState: readAutosaveState
    };
});
