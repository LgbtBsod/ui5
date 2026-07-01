sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants"
], function (MessageCodeConstants) {
    "use strict";

    var TECHNICAL_CODES = MessageCodeConstants.TECHNICAL;

    function decodeBase64(sBase64) {
        if (typeof window !== "undefined" && typeof window.atob === "function") {
            return window.atob(sBase64);
        }
        if (typeof atob === "function") {
            return atob(sBase64);
        }
        throw new Error(TECHNICAL_CODES.BASE64_DECODE_UNAVAILABLE);
    }

    function base64ToHex(sBase64) {
        if (!sBase64) {
            return "";
        }

        var sBinary = decodeBase64(String(sBase64));
        if (typeof Uint8Array === "undefined") {
            // Legacy fallback: char-by-char hex with manual zero-pad
            var aLegacy = [];
            for (var i = 0; i < sBinary.length; i += 1) {
                aLegacy.push(sBinary.charCodeAt(i).toString(16).padStart(2, "0"));
            }
            return aLegacy.join("");
        }

        // Native path: convert each byte to a 2-digit hex pair.
        return Array.prototype.map.call(sBinary, function (sChar) {
            return sChar.charCodeAt(0).toString(16).padStart(2, "0");
        }).join("");
    }

    return Object.freeze({
        base64ToHex: base64ToHex
    });
});
