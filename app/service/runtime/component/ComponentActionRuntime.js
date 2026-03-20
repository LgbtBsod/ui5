sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver"
], function (NavigationIntentService, EffectTextResolver) {
    "use strict";

    function buildActionValidators(ActionContract) {
        var mValidators = {};
        var mActions = (ActionContract && ActionContract.ACTIONS) || {};
        var fnNormalize = ActionContract && ActionContract.normalizeActionPayload;

        if (typeof fnNormalize !== "function") {
            return mValidators;
        }

        [
            mActions.DETAIL_RETRY_GUARDED_SAVE,
            mActions.DETAIL_TAKEOVER_LOCK,
            mActions.DETAIL_CANCEL_ENTER_EDIT
        ].forEach(function (sAction) {
            if (!sAction) {
                return;
            }
            mValidators[sAction] = function (mPayload) {
                return fnNormalize(sAction, mPayload);
            };
        });

        return mValidators;
    }

    function registerDefaultHandlers(mOptions) {
        var oActionDispatcher = mOptions.actionDispatcher;
        var oActionContract = mOptions.actionContract || {};
        var oDetailFacade = mOptions.detailFacade;
        var fnRunGuardedSave = mOptions.runGuardedSave;
        var fnBuildLatestCtx = mOptions.buildLatestCtx;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnGetCtx = mOptions.getCtx;
        var mActions = oActionContract.ACTIONS || {};

        if (!oActionDispatcher || typeof oActionDispatcher.register !== "function") {
            return;
        }

        if (mActions.DETAIL_RETRY_GUARDED_SAVE) {
            oActionDispatcher.register(mActions.DETAIL_RETRY_GUARDED_SAVE, function () {
                return fnRunGuardedSave();
            });
        }
        if (mActions.DETAIL_TAKEOVER_LOCK) {
            oActionDispatcher.register(mActions.DETAIL_TAKEOVER_LOCK, function (mPayload) {
                return oDetailFacade.confirmTakeover(mPayload || {}, fnBuildLatestCtx()).then(fnApplyFacadeResult);
            });
        }
        if (mActions.DETAIL_CANCEL_ENTER_EDIT) {
            oActionDispatcher.register(mActions.DETAIL_CANCEL_ENTER_EDIT, function (mPayload) {
                return oDetailFacade.cancelEnterEdit(mPayload || {}, fnGetCtx()).then(fnApplyFacadeResult);
            });
        }
    }

    function createBundleText(component) {
        return function (sKey, aArgs) {
            return EffectTextResolver.getText(component, sKey, aArgs || [], sKey);
        };
    }

    function createApplyFacadeResult(mOptions) {
        var component = mOptions.component;
        var effectApplier = mOptions.effectApplier;
        var actionDispatcher = mOptions.actionDispatcher;
        var selectedModel = mOptions.selectedModel;
        var uiStateModel = mOptions.uiStateModel;
        var componentRuntimeSupport = mOptions.componentRuntimeSupport;
        var resolveBundleText = createBundleText(component);

        return function (oResult) {
            effectApplier.applyEffects(component, oResult && oResult.effects, {
                resolveTextKey: function (sKey) {
                    return resolveBundleText(sKey, []);
                },
                actionDispatcher: actionDispatcher
            });
            componentRuntimeSupport.syncDetailCurrentFromSelected(selectedModel, uiStateModel);
        };
    }

    function queuePendingNavigationIntent(component, oStateModel, StatePaths, oRouteEvent) {
        NavigationIntentService.queuePendingIntent(component, oStateModel, StatePaths, oRouteEvent);
    }

    function clearPendingNavigationIntent(oStateModel, StatePaths) {
        NavigationIntentService.clearPendingIntent(oStateModel, StatePaths);
    }

    function revertPendingNavigationIntent(component, oStateModel, StatePaths) {
        return NavigationIntentService.revertPendingIntent(component, oStateModel, StatePaths);
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        return NavigationIntentService.resumePendingIntent(component, oStateModel, StatePaths);
    }

    function restorePendingNavigationIntent(component, oStateModel, StatePaths) {
        return NavigationIntentService.restorePendingIntent(component, oStateModel, StatePaths);
    }

    return {
        buildActionValidators: buildActionValidators,
        registerDefaultHandlers: registerDefaultHandlers,
        createBundleText: createBundleText,
        createApplyFacadeResult: createApplyFacadeResult,
        queuePendingNavigationIntent: queuePendingNavigationIntent,
        clearPendingNavigationIntent: clearPendingNavigationIntent,
        revertPendingNavigationIntent: revertPendingNavigationIntent,
        resumePendingNavigationIntent: resumePendingNavigationIntent,
        restorePendingNavigationIntent: restorePendingNavigationIntent
    };
});
