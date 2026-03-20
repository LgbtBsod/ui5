sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/FrontendVariablesSchema"
], function (FrontendVariablesSchema) {
    "use strict";

    return Object.freeze({
        SETTINGS_CACHE_TTL_MS: 5 * 60 * 1000,
        SETTINGS_CACHE_STAMP_TOLERANCE_MS: 5 * 60 * 1000,
        FALLBACKS: Object.freeze({
            FRONTEND_VARIABLES: FrontendVariablesSchema.buildDefaults()
        }),
        SOURCES: Object.freeze({
            RUNTIME_SETTINGS_GLOBAL: "RuntimeSettingsSet(GLOBAL)"
        })
    });
});
