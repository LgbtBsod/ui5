sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentNavigationGuardDecisionRuntime"
], function (
    ComponentNavigationGuardDecisionRuntime
) {
    "use strict";

    function attachBeforeRouteMatched(mOptions) {
        var oComponent = mOptions.component;
        var oRouter = oComponent.getRouter();

        if (oComponent._oLifecycleRouter && oComponent._fnBeforeRouteMatched && oComponent._oLifecycleRouter.detachBeforeRouteMatched) {
            oComponent._oLifecycleRouter.detachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        }

        oComponent._oLifecycleRouter = oRouter;
        oComponent._fnBeforeRouteMatched = function (oEvent) {
            return ComponentNavigationGuardDecisionRuntime.handleBeforeRouteMatched(mOptions, oEvent);
        };

        oRouter.attachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
    }

    return {
        attachBeforeRouteMatched: attachBeforeRouteMatched
    };
});
