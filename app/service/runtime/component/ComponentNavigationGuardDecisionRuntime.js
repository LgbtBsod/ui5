sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/WorkflowDecisionRuntime"
], function (
    ModelStateRuntime,
    NavigationContracts,
    WorkflowContracts,
    ComponentListenerContracts,
    JsRuntime,
    BehaviorScopes,
    WorkflowDecisionRuntime
) {
    "use strict";

    var PATHS = ComponentListenerContracts.PATHS;
    var VALUES = ComponentListenerContracts.VALUES;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var oWorkflowScope = BehaviorScopes.workflow;

    function normalizeRouteName(vRouteName) {
        return String(vRouteName || "").trim();
    }

    function normalizeId(vId) {
        return String(vId || "").trim();
    }

    function isDetailRoute(sRouteName) {
        return sRouteName === NavigationContracts.ROUTES.DETAIL;
    }

    function resolveCurrentDbKey(oStateModel) {
        return normalizeId(
            ModelStateRuntime.readOnModel(oStateModel, "/postOpenHydratedRootId", "") ||
            ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") ||
            ModelStateRuntime.readOnModel(oStateModel, PATHS.SELECTED_ID, "")
        );
    }

    function resolveNextRouteIntent(oRouteEvent) {
        var mArgs = oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("arguments") || {};

        return {
            routeName: normalizeRouteName(oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("name")),
            routeArgs: mArgs,
            dbKey: normalizeId(mArgs && mArgs.id)
        };
    }

    function isSameDetailTarget(oStateModel, oRouteEvent) {
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);
        var sCurrentDbKey = resolveCurrentDbKey(oStateModel);

        return !!(sCurrentDbKey && isDetailRoute(oNextIntent.routeName) && oNextIntent.dbKey === sCurrentDbKey);
    }

    function shouldGuardDetailNavigation(oStateModel, oRouteEvent) {
        var sCurrentDbKey = resolveCurrentDbKey(oStateModel);
        var oNextIntent = resolveNextRouteIntent(oRouteEvent);

        return !!(sCurrentDbKey && !isSameDetailTarget(oStateModel, oRouteEvent) && oNextIntent.routeName);
    }

    function shouldReleaseDetailLock(oStateModel, oRouteEvent, StatePaths) {
        var sMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));

        if (!shouldGuardDetailNavigation(oStateModel, oRouteEvent)) {
            return false;
        }

        return WorkflowContracts.isEditableMode(sMode) || sLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
    }

    function consumeNavGuardBypass(oStateModel) {
        if (!ModelStateRuntime.readOnModel(oStateModel, PATHS.NAV_GUARD_BYPASS, false)) {
            return false;
        }

        ModelStateRuntime.writeOnModel(oStateModel, PATHS.NAV_GUARD_BYPASS, false);
        return true;
    }

    function preventAndQueueNavigation(oEvent, mOptions) {
        oEvent.preventDefault();
        mOptions.queuePendingNavigationIntent(oEvent, {
            owner: "navigationGuard",
            resumeMode: "afterGuardedSave"
        });

        if (typeof mOptions.revertPendingNavigationIntent === TYPE_FUNCTION) {
            mOptions.revertPendingNavigationIntent();
        }
    }

    function handleSaveInFlightNavigation(oEvent, mOptions, oStateModel, StatePaths) {
        if (!ModelStateRuntime.readOnModel(oStateModel, StatePaths.SAVE_IN_FLIGHT, false)) {
            return false;
        }

        preventAndQueueNavigation(oEvent, mOptions);
        return true;
    }

    function handleDirtyNavigation(oComponent, oEvent, mOptions, oStateModel, StatePaths) {
        var oBlockedIntent;

        if (!ModelStateRuntime.readOnModel(oStateModel, PATHS.IS_DIRTY, false) || !shouldGuardDetailNavigation(oStateModel, oEvent)) {
            return false;
        }

        oBlockedIntent = resolveNextRouteIntent(oEvent);
        preventAndQueueNavigation(oEvent, mOptions);

        oWorkflowScope.execute("confirmUnsavedAndHandle", {
            controller: {
                getModel: oComponent.getModel.bind(oComponent),
                getResourceBundle: function () {
                    return oComponent.getModel(ComponentListenerContracts.MODEL_NAMES.I18N).getResourceBundle();
                }
            },
            onSave: function () {
                return mOptions.runGuardedSave({ resumePendingNavigation: true });
            },
            onCancel: function () {
                if (typeof mOptions.restorePendingNavigationIntent === TYPE_FUNCTION) {
                    return mOptions.restorePendingNavigationIntent();
                }
                if (typeof mOptions.clearPendingNavigationIntent === TYPE_FUNCTION) {
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
                oComponent.getRouter()[METHODS.NAV_TO](
                    oPending.routeName || oBlockedIntent.routeName,
                    oPending.routeArgs || oBlockedIntent.routeArgs || {},
                    false
                );
                return;
            }

            if (sDecision === VALUES.NO_CHANGES) {
                mOptions.resumePendingNavigationIntent();
                return;
            }

            if (sDecision === WorkflowDecisionRuntime.RESULTS.SAVE_FAILED && typeof mOptions.clearPendingNavigationIntent === TYPE_FUNCTION) {
                mOptions.clearPendingNavigationIntent();
            }
        });

        return true;
    }

    function handleLockReleaseNavigation(oComponent, oEvent, mOptions, oStateModel, StatePaths) {
        if (!shouldReleaseDetailLock(oStateModel, oEvent, StatePaths)) {
            return false;
        }

        oEvent.preventDefault();
        mOptions.queuePendingNavigationIntent(oEvent);

        Promise.resolve(oWorkflowScope.execute("releaseWithTrySave", {
            controller: {
                getModel: oComponent.getModel.bind(oComponent)
            }
        })).finally(function () {
            mOptions.resetDetailNavigationState(oComponent);
            mOptions.resumePendingNavigationIntent();
        });

        return true;
    }

    function handleBeforeRouteMatched(mOptions, oEvent) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths || {};

        if (consumeNavGuardBypass(oStateModel)) {
            return;
        }
        if (handleSaveInFlightNavigation(oEvent, mOptions, oStateModel, StatePaths)) {
            return;
        }
        if (handleDirtyNavigation(oComponent, oEvent, mOptions, oStateModel, StatePaths)) {
            return;
        }
        if (handleLockReleaseNavigation(oComponent, oEvent, mOptions, oStateModel, StatePaths)) {
            return;
        }

        mOptions.resetDetailAccessGuard(oStateModel);
    }

    return {
        handleBeforeRouteMatched: handleBeforeRouteMatched,
        resolveNextRouteIntent: resolveNextRouteIntent,
        shouldGuardDetailNavigation: shouldGuardDetailNavigation,
        shouldReleaseDetailLock: shouldReleaseDetailLock
    };
});
