sap.ui.define([], function () {
    "use strict";

    function decodeBase64(sBase64) {
        if (typeof window !== "undefined" && typeof window.atob === "function") {
            return window.atob(sBase64);
        }
        if (typeof atob === "function") {
            return atob(sBase64);
        }
        throw new Error("base64_decode_unavailable");
    }

    function base64ToHex(sBase64) {
        var sBinary;
        var aHex = [];
        var i;
        var sHex;

        if (!sBase64) {
            return "";
        }

        sBinary = decodeBase64(String(sBase64));
        for (i = 0; i < sBinary.length; i += 1) {
            sHex = sBinary.charCodeAt(i).toString(16);
            aHex.push(sHex.length >= 2 ? sHex : "0" + sHex);
        }
        return aHex.join("");
    }

    return Object.freeze({
        base64ToHex: base64ToHex
    });
});
