sap.ui.define([
    "checklist/app/service/framework/NavigationIntentService"
], function (NavigationIntentService) {
    "use strict";

    function createBundleText(component) {
        return function (sKey, aArgs) {
            var oBundle = component.getModel("i18n") && component.getModel("i18n").getResourceBundle();
            return oBundle && oBundle.hasText && oBundle.hasText(sKey) ? oBundle.getText(sKey, aArgs || []) : sKey;
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

    function queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent) {
        NavigationIntentService.queuePendingIntent(oStateModel, StatePaths, oRouteEvent);
    }

    function clearPendingNavigationIntent(oStateModel, StatePaths) {
        NavigationIntentService.clearPendingIntent(oStateModel, StatePaths);
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        return NavigationIntentService.resumePendingIntent(component, oStateModel, StatePaths);
    }

    return {
        clearPendingNavigationIntent: clearPendingNavigationIntent,
        createApplyFacadeResult: createApplyFacadeResult,
        createBundleText: createBundleText,
        queuePendingNavigationIntent: queuePendingNavigationIntent,
        resumePendingNavigationIntent: resumePendingNavigationIntent
    };
});
