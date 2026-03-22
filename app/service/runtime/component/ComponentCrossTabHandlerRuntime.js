sap.ui.define([], function () {
    "use strict";

    function register(mOptions) {
        var oCrossTabRuntime = mOptions.attachCrossTabRuntime({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            statePaths: mOptions.statePaths,
            bundleText: mOptions.bundleText,
            setGlobalBanner: mOptions.setGlobalBanner,
            handleForceReadOnly: mOptions.handleForceReadOnly
        });

        return {
            crossTabRuntime: oCrossTabRuntime,
            publishTabSignal: oCrossTabRuntime.publishTabSignal
        };
    }

    return {
        register: register
    };
});
