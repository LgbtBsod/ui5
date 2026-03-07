sap.ui.define([
    "sap_ui5/infra/adapters/Ui5StyleAdapter"
], function (Ui5StyleAdapter) {
    "use strict";

    function splitClassNames(sClassNames) {
        return String(sClassNames || "").split(/\s+/).map(function (sName) {
            return sName.trim();
        }).filter(Boolean);
    }

    function applyDirect(oTarget, sClassNames, sMethodName) {
        if (!oTarget || typeof oTarget[sMethodName] !== "function") {
            return oTarget;
        }
        splitClassNames(sClassNames).forEach(function (sClassName) {
            oTarget[sMethodName](sClassName);
        });
        return oTarget;
    }

    function forceReflow(oTarget) {
        var oDomRef = oTarget && oTarget.getDomRef && oTarget.getDomRef();
        if (oDomRef) {
            void oDomRef.offsetWidth;
        }
    }

    return {
        enable: function (oTarget, sClassNames) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.enableOn === "function") {
                return Ui5StyleAdapter.enableOn(oTarget, sClassNames);
            }
            return applyDirect(oTarget, sClassNames, "addStyleClass");
        },
        disable: function (oTarget, sClassNames) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.disableOn === "function") {
                return Ui5StyleAdapter.disableOn(oTarget, sClassNames);
            }
            return applyDirect(oTarget, sClassNames, "removeStyleClass");
        },
        restart: function (oTarget, sClassName) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.restartOn === "function") {
                return Ui5StyleAdapter.restartOn(oTarget, sClassName);
            }
            applyDirect(oTarget, sClassName, "removeStyleClass");
            forceReflow(oTarget);
            return applyDirect(oTarget, sClassName, "addStyleClass");
        }
    };
});
