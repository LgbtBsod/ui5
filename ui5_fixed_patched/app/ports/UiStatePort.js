sap.ui.define([], function () {
    "use strict";

    /**
     * @interface UiStatePort
     */
    return {
        /** @param {string} sModelName @param {string} sPath @returns {*} */
        get: function (sModelName, sPath) {},
        /** @param {string} sModelName @param {string} sPath @param {*} vValue */
        set: function (sModelName, sPath, vValue) {},
        /** @param {string} sModelName @param {string} sPath @param {Object} oPartial */
        merge: function (sModelName, sPath, oPartial) {}
    };
});
