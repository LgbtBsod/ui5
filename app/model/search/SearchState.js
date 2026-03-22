sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts"
], function (StatePaths, SearchRuntimeConstants) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeConstants.SEARCH_MODE;
    var SEARCH_SEGMENTS = SearchRuntimeConstants.SEARCH_SEGMENTS;

    return {
        applyDefaults: function (oStateModel) {
            if (!oStateModel || typeof oStateModel.getProperty !== "function") {
                return;
            }
            if (!oStateModel.getProperty("/search") || typeof oStateModel.getProperty("/search") !== "object") {
                oStateModel.setProperty("/search", {});
            }
            if (oStateModel.getProperty(StatePaths.SEARCH_CHECKS_FAIL_SEGMENT) === undefined) {
                oStateModel.setProperty(StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, SEARCH_SEGMENTS.ALL);
            }
            if (oStateModel.getProperty(StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT) === undefined) {
                oStateModel.setProperty(StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, SEARCH_SEGMENTS.ALL);
            }
            if (oStateModel.getProperty(StatePaths.SEARCH_MODE) === undefined) {
                oStateModel.setProperty(StatePaths.SEARCH_MODE, SEARCH_MODE.EXACT);
            }
        }
    };
});
