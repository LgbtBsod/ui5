sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport"
], function (SpreadsheetExport) {
    "use strict";

    return {
        // Deprecated compatibility wrapper. New code should import
        // PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport directly.
        download: function (sFileName, aRows, mSettings) {
            return SpreadsheetExport.download(sFileName, aRows, mSettings);
        }
    };
});
