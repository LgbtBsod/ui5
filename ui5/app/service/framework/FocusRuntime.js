sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (SchedulingRuntime) {
    "use strict";

    function focusSoon(oControl) {
        if (!oControl || typeof oControl.focus !== "function") {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oControl.focus();
        }, 0);
        return true;
    }

    return {
        focusSoon: focusSoon
    };
});
