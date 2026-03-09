sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime"
], function (ModelStateRuntime) {
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
            ModelStateRuntime.readOnModel(oStateModel, "/mode", ""),
            sFallback || "READ"
        );
    }

    function readLockState(oStateModel, sFallback) {
        return normalizeState(
            ModelStateRuntime.readOnModel(oStateModel, "/lockOperationState", ""),
            sFallback || "IDLE"
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
