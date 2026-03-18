sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime"
], function (ControllerViewStateRuntime) {
    "use strict";

    function withActionBusy(oController, sViewBusyPath, fnAction, fnSyncControlBusy) {
        if (typeof fnSyncControlBusy === "function") {
            fnSyncControlBusy(true);
        }
        return ControllerViewStateRuntime.withFlag(oController, sViewBusyPath, function () {
            return typeof fnAction === "function" ? fnAction() : undefined;
        }).finally(function () {
            if (typeof fnSyncControlBusy === "function") {
                fnSyncControlBusy(false);
            }
        });
    }

    return {
        withActionBusy: withActionBusy
    };
});
