sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ModelStateRuntime, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;
    var PHONE_MAX_WIDTH = 720;
    var TABLET_MAX_WIDTH = 1080;

    function resolveViewportWidth() {
        return Math.max(window.innerWidth || 0, document.documentElement && document.documentElement.clientWidth || 0, 0);
    }

    function syncResponsiveViewport(oController) {
        var iWidth = resolveViewportWidth();
        var bPhone = iWidth > 0 && iWidth <= PHONE_MAX_WIDTH;
        var bTablet = iWidth > PHONE_MAX_WIDTH && iWidth <= TABLET_MAX_WIDTH;

        ModelStateRuntime.write(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_IS_PHONE_VIEWPORT, bPhone);
        ModelStateRuntime.write(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_IS_TABLET_VIEWPORT, bTablet);
        ModelStateRuntime.write(oController, APP_VIEW_MODEL, MODEL_PATHS.APP_VIEW_VIEWPORT_WIDTH, iWidth);

        if (typeof oController._syncShellMetrics === "function") {
            oController._syncShellMetrics();
        }
        if (typeof oController._syncLayoutViewportGeometry === "function") {
            oController._syncLayoutViewportGeometry();
        }
    }

    return {
        syncResponsiveViewport: syncResponsiveViewport
    };
});
