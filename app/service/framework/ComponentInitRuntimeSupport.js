sap.ui.define([], function () {
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
        oStateModel.setProperty(StatePaths.PENDING_NAVIGATION_INTENT, {
            routeName: oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("name"),
            routeArgs: (oRouteEvent && oRouteEvent.getParameter && oRouteEvent.getParameter("arguments")) || {},
            queuedAt: new Date().toISOString()
        });
    }

    function clearPendingNavigationIntent(oStateModel, StatePaths) {
        oStateModel.setProperty(StatePaths.PENDING_NAVIGATION_INTENT, null);
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        var oIntent = oStateModel.getProperty(StatePaths.PENDING_NAVIGATION_INTENT);
        if (!oIntent || !oIntent.routeName) {
            return false;
        }
        clearPendingNavigationIntent(oStateModel, StatePaths);
        oStateModel.setProperty("/navGuardBypass", true);
        component.getRouter().navTo(oIntent.routeName, oIntent.routeArgs || {}, false);
        return true;
    }

    return {
        clearPendingNavigationIntent: clearPendingNavigationIntent,
        createApplyFacadeResult: createApplyFacadeResult,
        createBundleText: createBundleText,
        queuePendingNavigationIntent: queuePendingNavigationIntent,
        resumePendingNavigationIntent: resumePendingNavigationIntent
    };
});
