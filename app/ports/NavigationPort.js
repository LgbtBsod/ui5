sap.ui.define([], function () {
    "use strict";

    /**
     * @interface NavigationPort
     * Optional when navigation is handled by Effects.
     */
    return {
        /** @param {string} sRoute @param {Object} oParams @param {boolean} bReplace */
        navTo: function (sRoute, oParams, bReplace) {}
    };
});
