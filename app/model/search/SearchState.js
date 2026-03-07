sap.ui.define([], function () {
    "use strict";

    return {
        applyDefaults: function (oStateModel) {
            if (!oStateModel.getProperty("/search") || typeof oStateModel.getProperty("/search") !== "object") {
                oStateModel.setProperty("/search", {});
            }
            if (!oStateModel.getProperty("/search/checksFailSegment")) {
                oStateModel.setProperty("/search/checksFailSegment", "ALL");
            }
            if (!oStateModel.getProperty("/search/barriersFailSegment")) {
                oStateModel.setProperty("/search/barriersFailSegment", "ALL");
            }
            if (!oStateModel.getProperty("/search/modeSwitch")) {
                oStateModel.setProperty("/search/modeSwitch", oStateModel.getProperty("/searchMode") || "EXACT");
            }
        }
    };
});
