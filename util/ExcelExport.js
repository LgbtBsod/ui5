sap.ui.define([], function () {
    "use strict";

    function _escapeXml(v) {
        return String(v == null ? "" : v)
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&apos;");
    }

    function _toRowXml(aValues) {
        return "<Row>" + aValues.map(function (vValue) {
            return "<Cell><Data ss:Type=\"String\">" + _escapeXml(vValue) + "</Data></Cell>";
        }).join("") + "</Row>";
    }

    function _buildWorkbookXml(aRows) {
        var aSafeRows = Array.isArray(aRows) ? aRows : [];
        var aColumns = aSafeRows.length ? Object.keys(aSafeRows[0]) : [];
        var sHeader = _toRowXml(aColumns);
        var sBody = aSafeRows.map(function (oRow) {
            return _toRowXml(aColumns.map(function (sColumn) { return oRow[sColumn]; }));
        }).join("");

        return [
            "<?xml version=\"1.0\"?>",
            "<?mso-application progid=\"Excel.Sheet\"?>",
            "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"",
            " xmlns:o=\"urn:schemas-microsoft-com:office:office\"",
            " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"",
            " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\">",
            "<Worksheet ss:Name=\"Export\"><Table>",
            sHeader,
            sBody,
            "</Table></Worksheet></Workbook>"
        ].join("");
    }

    return {
        download: function (sFileName, aRows) {
            var sXml = _buildWorkbookXml(aRows || []);
            var oBlob = new Blob([sXml], { type: "application/vnd.ms-excel;charset=utf-8" });
            var sUrl = URL.createObjectURL(oBlob);
            var oLink = document.createElement("a");

            oLink.href = sUrl;
            oLink.download = (sFileName || "report") + ".xls";
            document.body.appendChild(oLink);
            oLink.click();
            document.body.removeChild(oLink);
            URL.revokeObjectURL(sUrl);
        }
    };
});
