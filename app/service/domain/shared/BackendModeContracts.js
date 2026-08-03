sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (StatePaths) {
    "use strict";

    return Object.freeze({
        MODES: Object.freeze({
            REAL: "real"
        }),
        PATHS: Object.freeze({
            BACKEND_MODE: StatePaths.BACKEND_MODE,
            BACKEND_SERVICE_URL: "/backendServiceUrl",
            MAIN_SERVICE_METADATA_ERROR: "/mainServiceMetadataError",
            MAIN_SERVICE_METADATA_OK: "/mainServiceMetadataOk"
        }),
        CAPABILITY: Object.freeze({
            DEGRADED: "degraded",
            METADATA_UNAVAILABLE: "metadata_unavailable",
            PENDING: "pending",
            READY: "ready"
        })
    });
});
