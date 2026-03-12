sap.ui.define([], function () {
    "use strict";

    /**
     * @interface TelemetryPort
     */
    return {
        /** @param {{event:string,payload:Object}} mArgs @returns {void} */
        track: function (mArgs) {},
        /** @param {{context:Object}} mArgs @returns {Promise<{ok:true}>} */
        snapshot: function (mArgs) {}
    };
});
