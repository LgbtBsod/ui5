sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts"
], function (ModelStateRuntime, StatePaths, NavigationContracts, WorkflowContracts) {
    "use strict";

    function normalizeLayout(vLayout) {
        var sLayout = String(vLayout || "").trim();
        if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            return NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN;
        }
        if (sLayout === NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED || sLayout === NavigationContracts.LAYOUTS.TWO_COLUMNS_BEGIN_EXPANDED) {
            return NavigationContracts.LAYOUTS.TWO_COLUMNS_MID_EXPANDED;
        }
        return NavigationContracts.LAYOUTS.ONE_COLUMN;
    }

    function toLayoutKind(vLayout) {
        var sLayout = normalizeLayout(vLayout);
        if (sLayout === NavigationContracts.LAYOUTS.MID_COLUMN_FULL_SCREEN) {
            return "detailOnly";
        }
        if (sLayout === NavigationContracts.LAYOUTS.ONE_COLUMN) {
            return "single";
        }
        return "split";
    }

    function normalizeMode(vMode, sFallback) {
        return String(vMode || sFallback || WorkflowContracts.EDIT_MODES.READ).toUpperCase();
    }

    function normalizeState(vValue, sFallback) {
        return String(vValue || sFallback || "").toUpperCase();
    }

    function readLayout(oStateModel, sFallback) {
        return normalizeLayout(
            ModelStateRuntime.readOnModel(oStateModel, "/layout", sFallback || NavigationContracts.LAYOUTS.ONE_COLUMN)
        );
    }

    function readMode(oStateModel, sFallback) {
        return normalizeMode(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, ""),
            sFallback || WorkflowContracts.EDIT_MODES.READ
        );
    }

    function readLockState(oStateModel, sFallback) {
        return normalizeState(
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, ""),
            sFallback || WorkflowContracts.LOCK_STATES.READ_ONLY
        );
    }

    function readAutosaveState(oStateModel, sFallback) {
        return normalizeState(
            ModelStateRuntime.readOnModel(oStateModel, "/autosaveState", ""),
            sFallback || WorkflowContracts.AUTOSAVE_STATES.IDLE
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
