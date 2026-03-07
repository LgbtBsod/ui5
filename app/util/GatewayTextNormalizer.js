sap.ui.define([], function () {
    "use strict";

    var MOJIBAKE_MARKER_PATTERN = /[\u00D0\u00D1\u00C3\u00C2]/;
    var CYRILLIC_PATTERN = /[\u0400-\u04FF]/g;
    var MOJIBAKE_SCORE_PATTERN = /[\u00D0\u00D1\u00C3\u00C2]/g;
    var WINDOWS_1252_REVERSE_MAP = {
        338: 140,
        339: 156,
        352: 138,
        353: 154,
        376: 159,
        381: 142,
        382: 158,
        402: 131,
        710: 136,
        732: 152,
        8211: 150,
        8212: 151,
        8216: 145,
        8217: 146,
        8218: 130,
        8220: 147,
        8221: 148,
        8222: 132,
        8224: 134,
        8225: 135,
        8226: 149,
        8230: 133,
        8240: 137,
        8249: 139,
        8250: 155,
        8364: 128,
        8482: 153
    };

    function countMatches(sValue, oPattern) {
        var aMatches = String(sValue || "").match(oPattern);
        return aMatches ? aMatches.length : 0;
    }

    function scoreText(sValue) {
        return (countMatches(sValue, CYRILLIC_PATTERN) * 2) - (countMatches(sValue, MOJIBAKE_SCORE_PATTERN) * 3);
    }

    function tryDecodeViaEscape(sValue) {
        try {
            return decodeURIComponent(escape(sValue));
        } catch (_oError) {
            return sValue;
        }
    }

    function tryDecodeViaTextDecoder(sValue) {
        var aBytes = [];
        var TextDecoderCtor = typeof TextDecoder === "function" ? TextDecoder : null;
        var iIndex;
        var iCode;
        var iByte;
        if (!TextDecoderCtor) {
            return sValue;
        }
        for (iIndex = 0; iIndex < sValue.length; iIndex += 1) {
            iCode = sValue.charCodeAt(iIndex);
            iByte = iCode <= 255 ? iCode : WINDOWS_1252_REVERSE_MAP[iCode];
            if (typeof iByte !== "number") {
                return sValue;
            }
            aBytes.push(iByte);
        }
        try {
            return new TextDecoderCtor("utf-8", { fatal: true }).decode(new Uint8Array(aBytes));
        } catch (_oError) {
            return sValue;
        }
    }

    function normalize(vValue) {
        var sValue = String(vValue == null ? "" : vValue).trim();
        var sDecoded;
        if (!sValue || !MOJIBAKE_MARKER_PATTERN.test(sValue)) {
            return sValue;
        }
        sDecoded = tryDecodeViaEscape(sValue);
        if (sDecoded === sValue) {
            sDecoded = tryDecodeViaTextDecoder(sValue);
        }
        if (!sDecoded || scoreText(sDecoded) <= scoreText(sValue)) {
            return sValue;
        }
        return String(sDecoded).trim();
    }

    return {
        normalize: normalize
    };
});
