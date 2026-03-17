sap.ui.define([], function () {
    "use strict";

    function stripDataUrlPrefix(sValue) {
        return String(sValue || "").replace(/^data:.*?;base64,/i, "").trim();
    }

    function isBlobLike(oValue) {
        return typeof Blob !== "undefined" && oValue instanceof Blob;
    }

    function tryReadEmbeddedBase64(oValue) {
        if (!oValue || typeof oValue !== "object") {
            return "";
        }
        return stripDataUrlPrefix(
            oValue._fileBase64 ||
            oValue.fileBase64 ||
            oValue.base64 ||
            oValue.value ||
            oValue.content ||
            oValue.dataUrl ||
            ""
        );
    }

    function fileToBase64(oFile) {
        return new Promise(function (resolve, reject) {
            var oReader;
            var sEmbeddedBase64 = tryReadEmbeddedBase64(oFile);
            if (sEmbeddedBase64) {
                resolve(sEmbeddedBase64);
                return;
            }
            if (!oFile || typeof FileReader === "undefined") {
                resolve("");
                return;
            }
            if (!isBlobLike(oFile)) {
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
        base64ToBlob: base64ToBlob,
        fileToBase64: fileToBase64,
        stripDataUrlPrefix: stripDataUrlPrefix
    };
});
