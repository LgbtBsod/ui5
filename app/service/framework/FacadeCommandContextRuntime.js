sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime"
], function (ControllerCommandContextRuntime) {
    "use strict";

    function buildDefaultCtx(oController) {
        return ControllerCommandContextRuntime.buildDefaultCtx(oController);
    }

    function buildSearchCtx(oController) {
        return ControllerCommandContextRuntime.buildSearchCtx(oController);
    }

    return Object.freeze({
        buildDefaultCtx: buildDefaultCtx,
        buildSearchCtx: buildSearchCtx
    });
});
