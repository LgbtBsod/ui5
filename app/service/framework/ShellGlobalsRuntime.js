sap.ui.define([], function () {
    "use strict";

    function getWindow() {
        return typeof window === "undefined" ? null : window;
    }

    function markAppReady() {
        var oWindow = getWindow();
        if (!oWindow || typeof oWindow.__ui5MarkAppReady !== "function") {
            return false;
        }
        oWindow.__ui5MarkAppReady();
        return true;
    }

    function getBackgroundRuntime() {
        var oWindow = getWindow();
        return oWindow && oWindow.Ui5Bg ? oWindow.Ui5Bg : null;
    }

    return {
        getBackgroundRuntime: getBackgroundRuntime,
        markAppReady: markAppReady
    };
});
