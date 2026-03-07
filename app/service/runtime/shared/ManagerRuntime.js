sap.ui.define([], function () {
    "use strict";

    function initEventProvider(EventProvider, oInstance, aArgs) {
        EventProvider.apply(oInstance, aArgs || []);
    }

    function readNumberOption(mOptions, sKey) {
        return Number(mOptions && mOptions[sKey]);
    }

    return {
        initEventProvider: initEventProvider,
        readNumberOption: readNumberOption
    };
});
