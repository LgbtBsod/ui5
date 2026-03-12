sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/runtime/FrontendVariablesSchema"
], function (FrontendVariablesSchema) {
    "use strict";

    return {
        FALLBACKS: {
            FRONTEND_VARIABLES: FrontendVariablesSchema.buildDefaults()
sap.ui.define([], function () {
    "use strict";

    return {
        SOURCES: {
            RUNTIME_SETTINGS_GLOBAL: "RuntimeSettingsSet(GLOBAL)"
        }
    };
});
