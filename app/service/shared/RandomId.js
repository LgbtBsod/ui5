sap.ui.define([], function () {
    "use strict";

    /**
     * Generates 6 bytes of cryptographic randomness, encoded as base-36 string.
     * Falls back to Math.random only when Web Crypto API is unavailable.
     * @returns {string} 8-11 character base-36 fragment
     */
    function cryptoFragment() {
        try {
            if (typeof window !== "undefined" && window.crypto && typeof window.crypto.getRandomValues === "function") {
                var aBytes = new Uint8Array(6);
                window.crypto.getRandomValues(aBytes);
                var sHex = "";
                for (var i = 0; i < aBytes.length; i++) {
                    sHex += aBytes[i].toString(16).padStart(2, "0");
                }
                return parseInt(sHex, 16).toString(36);
            }
        } catch (_e) {
            // fall through
        }
        return Math.random().toString(36).slice(2, 12);
    }

    /**
     * Generates a correlation ID suitable for X-Correlation-ID headers.
     * Format: "{prefix}-{timestamp36}-{random36}"
     * @param {string} [sPrefix="req"] - prefix segment (e.g. "req", "tel")
     * @returns {string}
     */
    function correlationId(sPrefix) {
        return [
            sPrefix || "req",
            Date.now().toString(36),
            cryptoFragment()
        ].join("-");
    }

    /**
     * Generates a random base-36 fragment for use in composite IDs.
     * @returns {string}
     */
    function randomFragment() {
        return cryptoFragment();
    }

    return {
        correlationId: correlationId,
        randomFragment: randomFragment,
        cryptoFragment: cryptoFragment
    };
});
