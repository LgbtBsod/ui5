sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts"
], function (SearchCommandPolicy, OperationSourceContracts) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function onChecksFailSegmentChange(oController, oEvent) {
        SearchCommandPolicy.buildFilter(oController, {
            intent: SEARCH_SOURCES.CHECKS_SEGMENT,
            key: oEvent.getParameter("key")
        });
    }

    function onBarriersFailSegmentChange(oController, oEvent) {
        SearchCommandPolicy.buildFilter(oController, {
            intent: SEARCH_SOURCES.BARRIERS_SEGMENT,
            key: oEvent.getParameter("key")
        });
    }

    return {
        onChecksFailSegmentChange: onChecksFailSegmentChange,
        onBarriersFailSegmentChange: onBarriersFailSegmentChange
    };
});
