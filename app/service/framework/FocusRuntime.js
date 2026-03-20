sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (SchedulingRuntime) {
    "use strict";

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode.focus !== "function") {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oNode.focus();
        }, 0);
        return true;
    }

    function focusSoon(oControl) {
        if (!oControl || typeof oControl.focus !== "function") {
            return false;
        }
        SchedulingRuntime.restartTimer(0, function () {
            oControl.focus();
        }, 0);
        return true;
    }

    function createActiveElementFallback() {
        var oActive;
        var sActiveId;
        if (typeof document === "undefined") {
            return null;
        }
        oActive = document.activeElement;
        sActiveId = oActive && oActive.id ? String(oActive.id) : "";
        if (sActiveId) {
            return {
                focus: function () {
                    var oNode = document.getElementById(sActiveId);
                    focusDomNode(oNode);
                }
            };
        }
        if (oActive && typeof oActive.focus === "function") {
            return oActive;
        }
        return null;
    }

    return {
        createActiveElementFallback: createActiveElementFallback,
        focusDomNode: focusDomNode,
        focusSoon: focusSoon
    };
});
