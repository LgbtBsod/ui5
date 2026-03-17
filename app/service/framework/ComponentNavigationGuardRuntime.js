sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerContracts"
], function (ModelStateRuntime, NavigationContracts, WorkflowContracts, ComponentListenerContracts) {
    "use strict";

    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;

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
        return normalizeId(ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") || ModelStateRuntime.readOnModel(oStateModel, PATHS.SELECTED_ID, ""));
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
        return !!(sCurrentRootId && !isSameDetailTarget(oStateModel, oRouteEvent) && oNextIntent.routeName);
    }

    function shouldReleaseDetailLock(oStateModel, oRouteEvent, StatePaths) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        if (!shouldGuardDetailNavigation(oStateModel, oRouteEvent)) {
            return false;
        }
        return WorkflowContracts.isEditableMode(sMode) || sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
    }

    function bindBeforeRouteMatched(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths || {};
        var oRouter = oComponent.getRouter();

        if (oComponent._oLifecycleRouter && oComponent._fnBeforeRouteMatched && oComponent._oLifecycleRouter.detachBeforeRouteMatched) {
            oComponent._oLifecycleRouter.detachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        }
        oComponent._oLifecycleRouter = oRouter;
        oComponent._fnBeforeRouteMatched = function (oEvent) {
            if (ModelStateRuntime.readOnModel(oStateModel, PATHS.NAV_GUARD_BYPASS, false)) {
                ModelStateRuntime.writeOnModel(oStateModel, PATHS.NAV_GUARD_BYPASS, false);
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false)) {
                oEvent.preventDefault();
                mOptions.queuePendingNavigationIntent(oEvent);
                if (typeof mOptions.revertPendingNavigationIntent === "function") {
                    mOptions.revertPendingNavigationIntent();
                }
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false) && shouldGuardDetailNavigation(oStateModel, oEvent)) {
                oEvent.preventDefault();
                mOptions.queuePendingNavigationIntent(oEvent);
                if (typeof mOptions.revertPendingNavigationIntent === "function") {
                    mOptions.revertPendingNavigationIntent();
                }
                mOptions.workflowCoordinator.confirmUnsavedAndHandle({
                    getModel: oComponent.getModel.bind(oComponent),
                    getResourceBundle: function () { return oComponent.getModel(ComponentListenerContracts.MODEL_NAMES.I18N).getResourceBundle(); }
                }, function () {
                    return mOptions.runGuardedSave();
                }, {
                    onCancel: function () {
                        if (typeof mOptions.restorePendingNavigationIntent === "function") {
                            return mOptions.restorePendingNavigationIntent();
                        }
                        if (typeof mOptions.clearPendingNavigationIntent === "function") {
                            return mOptions.clearPendingNavigationIntent();
                        }
                        return null;
                    }
                }).then(function (sDecision) {
                    if (sDecision === VALUES.DISCARD) {
                        var oPending = ModelStateRuntime.readOnModel(oStateModel, StatePaths.PENDING_NAVIGATION_INTENT, {}) || {};
                        mOptions.clearPendingNavigationIntent();
                        mOptions.resetDetailNavigationState(oComponent);
                        ModelStateRuntime.writeOnModel(oStateModel, PATHS.NAV_GUARD_BYPASS, true);
                        oComponent.getRouter().navTo(oPending.routeName || oEvent.getParameter("name"), oPending.routeArgs || oEvent.getParameter("arguments") || {}, false);
                        return;
                    }
                    if (sDecision === VALUES.SAVE || sDecision === VALUES.NO_CHANGES) {
                        mOptions.resumePendingNavigationIntent();
                    }
                });
                return;
            }
            if (shouldReleaseDetailLock(oStateModel, oEvent, StatePaths)) {
                oEvent.preventDefault();
                mOptions.queuePendingNavigationIntent(oEvent);
                Promise.resolve(mOptions.workflowCoordinator.releaseWithTrySave({ getModel: oComponent.getModel.bind(oComponent) })).finally(function () {
                    mOptions.resetDetailNavigationState(oComponent);
                    mOptions.resumePendingNavigationIntent();
                });
                return;
            }
            mOptions.resetDetailAccessGuard(oStateModel);
        };
        oRouter.attachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        oRouter.initialize();
    }

    return {
        attachBeforeRouteMatched: bindBeforeRouteMatched
    };
});
