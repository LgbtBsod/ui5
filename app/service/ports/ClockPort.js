sap.ui.define([], function () {
    "use strict";

    /**
     * @interface ClockPort
     */
    return {
        /** @returns {number} */
        now: function () {},
        /** @returns {string} */
        isoNow: function () {}
    };
});
