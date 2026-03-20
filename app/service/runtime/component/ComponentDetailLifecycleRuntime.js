sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (ModelStateRuntime, RootIdRuntime, NavigationContracts, WorkflowContracts, WorkflowRuntimeConstants) {
    "use strict";

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function normalizeId(vId) {
        return String(vId || "").trim();
    }

    function isDetailRoute(sRouteName) {
        return sRouteName === NavigationContracts.ROUTES.DETAIL || sRouteName === NavigationContracts.ROUTES.DETAIL_LAYOUT;
    }

    function resolveCurrentRootId(oStateModel) {
        return normalizeId(RootIdRuntime.resolveFromStateModel(oStateModel));
    }

    function resolveNextRouteIntent(oRouteEvent) {
        var mArgs = oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("arguments") || {};
        return {
            routeName: normalizeRouteName(oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("name")),
            routeArgs: mArgs,
            rootId: normalizeId(mArgs && mArgs.id)
        };
    }

    function isSameDetailTarget(oStateModel, oRouteEvent) {
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);
        var sCurrentRootId = resolveCurrentRootId(oStateModel);
        return !!(sCurrentRootId && isDetailRoute(oNextIntent.routeName) && oNextIntent.rootId === sCurrentRootId);
    }

    function shouldGuardDetailNavigation(oStateModel, oRouteEvent) {
        var sCurrentRootId = resolveCurrentRootId(oStateModel);
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);
        if (!sCurrentRootId || isSameDetailTarget(oStateModel, oRouteEvent) || !oNextIntent.routeName) {
            return false;
        }
        return true;
    }

    function shouldReleaseDetailLock(oStateModel, oRouteEvent, StatePaths) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        if (!shouldGuardDetailNavigation(oStateModel, oRouteEvent)) {
            return false;
        }
        return WorkflowContracts.isEditableMode(sMode) || sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
    }

    function syncDetailMeta(oStateModel, StatePaths) {
        var oReadiness = ModelStateRuntime.readOnModel(oStateModel, StatePaths.READINESS_DETAIL, {}) || {};
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        var sAutosaveState = WorkflowContracts.normalizeAutosaveState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE));
        var sValidationSource = String((ModelStateRuntime.readOnModel(oStateModel, StatePaths.VALIDATION_SUMMARY, {}) || {}).source || WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE);
        var bDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
        var bPermissionKnown = !!oReadiness.permissionKnown;
        var sReadinessStatus = String(oReadiness.status || WorkflowRuntimeConstants.READINESS_STATUS.IDLE).trim() || WorkflowRuntimeConstants.READINESS_STATUS.IDLE;
        var bAllowed = bPermissionKnown && sReadinessStatus !== WorkflowRuntimeConstants.READINESS_STATUS.DENIED && sReadinessStatus !== WorkflowRuntimeConstants.READINESS_STATUS.ERROR;
        ModelStateRuntime.writeOnModel(oStateModel, StatePaths.DETAIL_META, {
            rootId: String(oReadiness.rootId || RootIdRuntime.resolveActiveFromStateModel(oStateModel) || "").trim(),
            readiness: {
                status: sReadinessStatus,
                ready: !!oReadiness.ready,
                readyAt: String(oReadiness.readyAt || ""),
                error: String(oReadiness.error || "")
            },
            mode: sMode,
            lock: {
                state: sLockState,
                known: !!oReadiness.lockKnown
            },
            dirty: bDirty,
            permission: {
                known: bPermissionKnown,
                allowed: bAllowed
            },
            save: {
                state: sAutosaveState,
                lastSavedAt: ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null)
            },
            validation: {
                state: sValidationSource || WorkflowRuntimeConstants.VALIDATION_STATUS.IDLE
            }
        });
    }

    return {
        shouldReleaseDetailLock: shouldReleaseDetailLock,
        syncDetailMeta: syncDetailMeta
    };
});
