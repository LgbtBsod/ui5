sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport"
], function (SpreadsheetExport) {
    "use strict";

    return {
        download: function (sFileName, aRows, mSettings) {
            return SpreadsheetExport.download(sFileName, aRows, mSettings);
        }
    };
});
