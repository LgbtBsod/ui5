sap.ui.define([], function () {
    "use strict";

    var SCHEMA = {
        ENABLE_CLIENT_LOGS: {
            type: "boolean",
            defaultValue: false
        },
        MAX_SELECTION_SIZE: {
            type: "number",
            defaultValue: 500
        },
        DEFAULT_EXPORT_FORMAT: {
            type: "string",
            defaultValue: "xlsx"
        }
    };

    function cloneDefaults() {
        var mDefaults = {};
        Object.keys(SCHEMA).forEach(function (sKey) {
            mDefaults[sKey] = SCHEMA[sKey].defaultValue;
        });
        return mDefaults;
    }

    function coerceBoolean(vValue, bDefault) {
        if (typeof vValue === "boolean") {
            return vValue;
        }
        if (typeof vValue === "number") {
            if (vValue === 1) {
                return true;
            }
            if (vValue === 0) {
                return false;
            }
            return bDefault;
        }
        if (typeof vValue === "string") {
            var sNormalized = vValue.trim().toLowerCase();
            if (sNormalized === "true" || sNormalized === "1") {
                return true;
            }
            if (sNormalized === "false" || sNormalized === "0") {
                return false;
            }
        }
        return bDefault;
    }

    function coerceNumber(vValue, iDefault) {
        var iParsed = Number(vValue);
        if (Number.isFinite(iParsed)) {
            return iParsed;
        }
        return iDefault;
    }

    function coerceByType(vValue, oRule) {
        var vDefault = oRule.defaultValue;
        if (oRule.type === "boolean") {
            return coerceBoolean(vValue, vDefault);
        }
        if (oRule.type === "number") {
            return coerceNumber(vValue, vDefault);
        }
        if (oRule.type === "string") {
            if (vValue === null || vValue === undefined) {
                return vDefault;
            }
            return String(vValue);
        }
        return vDefault;
    }

    function sanitize(vRaw) {
        var mInput = vRaw && typeof vRaw === "object" ? vRaw : {};
        var mSanitized = {};

        Object.keys(SCHEMA).forEach(function (sKey) {
            var oRule = SCHEMA[sKey];
            var bHasKey = Object.prototype.hasOwnProperty.call(mInput, sKey);
            var vValue = bHasKey ? mInput[sKey] : oRule.defaultValue;
            mSanitized[sKey] = coerceByType(vValue, oRule);
        });

        return mSanitized;
    }

    return {
        SCHEMA: SCHEMA,
        buildDefaults: cloneDefaults,
        sanitize: sanitize
    };
});
