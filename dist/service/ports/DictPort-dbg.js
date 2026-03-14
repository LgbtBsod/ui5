sap.ui.define([], function () {
    "use strict";

    /**
     * @interface DictPort
     */
    return {
        /** @returns {Promise<{loaded:true}>} */
        ensureLoaded: function () {},
        /** @param {{type:string,code:string}} mArgs @returns {{text?:string}|null} */
        getItem: function (mArgs) {},
        /** @param {{type:string}} mArgs @returns {Array} */
        listByType: function (mArgs) {}
    };
});
