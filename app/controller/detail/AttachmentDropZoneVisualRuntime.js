sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime"
], function (ControlStyleRuntime) {
    "use strict";

    function setDropZoneClass(oController, sClassName, bActive) {
        var oDropZone = oController && oController.byId && oController.byId("attachmentDropZone");
        if (!oDropZone) {
            return;
        }
        if (bActive) {
            ControlStyleRuntime.enable(oDropZone, sClassName);
            return;
        }
        ControlStyleRuntime.disable(oDropZone, sClassName);
    }

    function setDropZoneState(oController, bPrimed, bActive) {
        setDropZoneClass(oController, "isAttachmentDropPrimed", !!bPrimed);
        setDropZoneClass(oController, "isAttachmentDropActive", !!bActive);
    }

    function resetVisual(oController) {
        setDropZoneState(oController, false, false);
    }

    return {
        resetVisual: resetVisual,
        setDropZoneState: setDropZoneState
    };
});
