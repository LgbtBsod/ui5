sap.ui.define([], function () {
    "use strict";

    function runLoad(mArgs) {
        return mArgs.runLifecycle({
            runLoadFlow: function () {
                return mArgs.runDialogLoad({
                    viewModel: mArgs.viewModel,
                    loadAnalytics: mArgs.loadAnalytics
                });
            },
            applyLoadError: mArgs.applyLoadError
        }).then(function (oLifecycleResult) {
            return (oLifecycleResult && oLifecycleResult.result) || null;
        });
    }

    return {
        runLoad: runLoad
    };
});
