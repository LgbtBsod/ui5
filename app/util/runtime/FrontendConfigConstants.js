sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/runtime/FrontendVariablesSchema"
], function (FrontendVariablesSchema) {
    "use strict";

    return {
        SETTINGS_CACHE_TTL_MS: 5 * 60 * 1000,
        FALLBACKS: {
            FRONTEND_VARIABLES: FrontendVariablesSchema.buildDefaults()
        },
        SOURCES: {
            RUNTIME_SETTINGS_GLOBAL: "RuntimeSettingsSet(GLOBAL)"
        }
    };
});
