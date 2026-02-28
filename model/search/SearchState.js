sap.ui.define([], function () {
    "use strict";

    return {
        applyDefaults: function (oStateModel) {
            if (!oStateModel.getProperty("/search") || typeof oStateModel.getProperty("/search") !== "object") {
                oStateModel.setProperty("/search", {});
            }
            if (!oStateModel.getProperty("/search/failSegment")) {
                oStateModel.setProperty("/search/failSegment", "ALL");
            }
            if (!oStateModel.getProperty("/search/barrierFailSegment")) {
                oStateModel.setProperty("/search/barrierFailSegment", "ALL");
            }
            if (!oStateModel.getProperty("/search/modeSwitch")) {
                oStateModel.setProperty("/search/modeSwitch", oStateModel.getProperty("/searchMode") || "EXACT");
            }
        }
    };
});
