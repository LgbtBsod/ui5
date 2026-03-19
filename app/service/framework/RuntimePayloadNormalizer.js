sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    var STRING_KEYS = Object.freeze([
        "rootId",
        "selectedRowId",
        "intent",
        "source",
        "entity",
        "op",
        "field",
        "key",
        "status",
        "id"
    ]);

    var BOOLEAN_KEYS = Object.freeze([
        "state",
        "silent",
        "userInitiated"
    ]);

    function normalize(mInput, mOptions) {
        var oInput = RuntimeInput.asObject(mInput);
        var oNormalized = Object.assign({}, oInput);
        var aStringKeys = (mOptions && mOptions.stringKeys) || STRING_KEYS;
        var aBooleanKeys = (mOptions && mOptions.booleanKeys) || BOOLEAN_KEYS;

        aStringKeys.forEach(function (sKey) {
            if (Object.prototype.hasOwnProperty.call(oInput, sKey)) {
                oNormalized[sKey] = RuntimeInput.asString(oInput[sKey]).trim();
            }
        });

        aBooleanKeys.forEach(function (sKey) {
            if (Object.prototype.hasOwnProperty.call(oInput, sKey)) {
                oNormalized[sKey] = RuntimeInput.asBoolean(oInput[sKey], false);
            }
        });

        return oNormalized;
    }

    return Object.freeze({
        normalize: normalize,
        STRING_KEYS: STRING_KEYS,
        BOOLEAN_KEYS: BOOLEAN_KEYS
    });
});
