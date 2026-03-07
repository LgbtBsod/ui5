sap.ui.define([], function () {
    "use strict";

    /**
     * @interface PersonSuggestPort
     * NOTE: "on interaction only" policy enforcement is handled in later phases.
     */
    return {
        /** @param {{query:string,limit:number}} mArgs @returns {Promise<{items:Array}>} */
        suggest: function (mArgs) {}
    };
});
