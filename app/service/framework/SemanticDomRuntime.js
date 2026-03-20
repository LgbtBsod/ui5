sap.ui.define([], function () {
    "use strict";

    function resolveDomRef(oControl) {
        return oControl && oControl.getDomRef && oControl.getDomRef();
    }

    function syncAttributes(oControl, mAttrs) {
        var oDomRef = resolveDomRef(oControl);
        Object.keys(mAttrs || {}).forEach(function (sAttr) {
            if (!oDomRef) {
                return;
            }
            if (mAttrs[sAttr]) {
                oDomRef.setAttribute(sAttr, mAttrs[sAttr]);
                return;
            }
            oDomRef.removeAttribute(sAttr);
        });
    }

    function syncControllerTarget(oController, sId, mAttrs) {
        var oControl = oController && oController.byId && oController.byId(sId);
        syncAttributes(oControl, mAttrs);
    }

    return {
        syncAttributes: syncAttributes,
        syncControllerTarget: syncControllerTarget
    };
});
