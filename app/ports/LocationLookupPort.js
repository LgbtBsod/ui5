sap.ui.define([], function () {
    "use strict";

    /**
     * @interface LocationLookupPort
     */
    return {
        /** @param {{query:string,limit:number}} mArgs @returns {Promise<{items:Array}>} */
        search: function (mArgs) {}
    };
});
