sap.ui.define([
    "sap/ui/export/Spreadsheet"
], function (Spreadsheet) {
    "use strict";

    function inferColumns(aRows) {
        var oFirstRow = Array.isArray(aRows) && aRows.length ? aRows[0] : null;
        return oFirstRow ? Object.keys(oFirstRow).map(function (sKey) {
            return { label: sKey, property: sKey, type: "string" };
        }) : [];
    }

    function download(sFileName, aRows, mSettings) {
        var aSafeRows = Array.isArray(aRows) ? aRows.slice() : [];
        var aColumns = mSettings && Array.isArray(mSettings.workbookColumns) && mSettings.workbookColumns.length
            ? mSettings.workbookColumns
            : inferColumns(aSafeRows);
        var oSpreadsheet = new Spreadsheet({
            workbook: { columns: aColumns },
            dataSource: aSafeRows,
            fileName: String(sFileName || "report") + ".xlsx"
        });

        return oSpreadsheet.build().finally(function () {
            oSpreadsheet.destroy();
        });
    }

    return {
        download: download
    };
});
