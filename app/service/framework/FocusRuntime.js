sap.ui.define([
    "checklist/app/service/framework/SchedulingRuntime"
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
