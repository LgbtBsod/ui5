sap.ui.define([], function () {
    "use strict";

    function focusSoon(oControl) {
        if (!oControl || typeof oControl.focus !== "function") {
            return false;
        }
        setTimeout(function () {
            oControl.focus();
        }, 0);
        return true;
    }

    return {
        focusSoon: focusSoon
    };
});
