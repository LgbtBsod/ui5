sap.ui.define([], function () {
    "use strict";

    function getWindow() {
        return typeof window === "undefined" ? null : window;
    }

    function getUshellContainer() {
        var oWindow = getWindow();
        var oSap = oWindow && oWindow.sap;
        var oUshell = oSap && oSap.ushell;
        return oUshell && oUshell.Container ? oUshell.Container : null;
    }

    function tryOfficialAppReady() {
        var oContainer = getUshellContainer();
        if (!oContainer || typeof oContainer.getServiceAsync !== "function") {
            return Promise.resolve(false);
        }
        return oContainer.getServiceAsync("AppLifeCycle").then(function (oService) {
            if (oService && typeof oService.markAppLoaded === "function") {
                oService.markAppLoaded();
                return true;
            }
            return false;
        }).catch(function () {
            return false;
        });
    }

    function markAppReady() {
        var oWindow = getWindow();
        return tryOfficialAppReady().then(function (bMarked) {
            if (bMarked) {
                return true;
            }
            if (!oWindow || typeof oWindow.__ui5MarkAppReady !== "function") {
                return false;
            }
            oWindow.__ui5MarkAppReady();
            return true;
        });
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
