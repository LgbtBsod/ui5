sap.ui.define([], function () {
    "use strict";

    function randomWord() {
        return Math.floor(Math.random() * 0x100000000);
    }

    function cryptoWordArray() {
        if (typeof window !== "undefined" && window.crypto && typeof window.crypto.getRandomValues === "function") {
            var aValues = new Uint32Array(4);
            window.crypto.getRandomValues(aValues);
            return Array.prototype.slice.call(aValues);
        }
        return [randomWord(), randomWord(), randomWord(), randomWord()];
    }

    function toHex32(aValues) {
        return (aValues || []).map(function (nValue) {
            var s = (Number(nValue) >>> 0).toString(16);
            while (s.length < 8) { s = "0" + s; }
            return s;
        }).join("").slice(0, 32).toUpperCase();
    }

    function createHex32() {
        return toHex32(cryptoWordArray());
    }

    return {
        createHex32: createHex32
    };
});
