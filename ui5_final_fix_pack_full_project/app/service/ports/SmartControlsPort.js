sap.ui.define([], function () {
    "use strict";

    /**
     * @interface SmartControlsPort
     */
    return {
        /** @returns {void} */
        rebindSearchTable: function () {},
        /** @returns {Object} */
        getSmartFilterData: function () {},
        /** @returns {string|null} */
        getSelectedRowKey: function () {},
        /** @returns {Array<object>} */
        getVisibleRows: function () {},
        /** @param {boolean} bValue @returns {void} */
        setTableBusy: function (bValue) {}
    };
});
