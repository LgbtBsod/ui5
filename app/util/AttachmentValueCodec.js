sap.ui.define([], function () {
    "use strict";

    function stripDataUrlPrefix(sValue) {
        return String(sValue || "").replace(/^data:.*?;base64,/i, "").trim();
    }

    function fileToBase64(oFile) {
        return new Promise(function (resolve, reject) {
            var oReader;
            if (!oFile || typeof FileReader === "undefined") {
                resolve("");
                return;
            }
            oReader = new FileReader();
            oReader.onload = function (oEvent) {
                resolve(stripDataUrlPrefix(oEvent && oEvent.target && oEvent.target.result));
            };
            oReader.onerror = function () {
                reject(new Error("ATTACHMENT_READ_FAILED"));
            };
            oReader.readAsDataURL(oFile);
        });
    }

    function base64ToBlob(sBase64, sMimeType) {
        var sBinary = typeof atob === "function" ? atob(stripDataUrlPrefix(sBase64)) : "";
        var iLength = sBinary.length;
        var aBytes = new Uint8Array(iLength);
        var iIndex;
        for (iIndex = 0; iIndex < iLength; iIndex += 1) {
            aBytes[iIndex] = sBinary.charCodeAt(iIndex);
        }
        return new Blob([aBytes], {
            type: String(sMimeType || "application/octet-stream").trim() || "application/octet-stream"
        });
    }

    return {
        fileToBase64: fileToBase64,
        base64ToBlob: base64ToBlob,
        stripDataUrlPrefix: stripDataUrlPrefix
    };
});
