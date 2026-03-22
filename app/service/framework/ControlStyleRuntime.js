sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StyleAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (Ui5StyleAdapter, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function splitClassNames(sClassNames) {
        return String(sClassNames || "").split(/\s+/).map(function (sName) {
            return sName.trim();
        }).filter(Boolean);
    }

    function applyDirect(oTarget, sClassNames, sMethodName) {
        if (!oTarget || typeof oTarget[sMethodName] !== TYPE_FUNCTION) {
            return oTarget;
        }
        splitClassNames(sClassNames).forEach(function (sClassName) {
            oTarget[sMethodName](sClassName);
        });
        return oTarget;
    }

    function forceReflow(oTarget) {
        var oDomRef = oTarget && oTarget[METHODS.GET_DOM_REF] && oTarget[METHODS.GET_DOM_REF]();
        if (oDomRef) {
            void oDomRef.offsetWidth;
        }
    }

    return {
        enable: function (oTarget, sClassNames) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.enableOn === TYPE_FUNCTION) {
                return Ui5StyleAdapter.enableOn(oTarget, sClassNames);
            }
            return applyDirect(oTarget, sClassNames, METHODS.ADD_STYLE_CLASS);
        },
        disable: function (oTarget, sClassNames) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.disableOn === TYPE_FUNCTION) {
                return Ui5StyleAdapter.disableOn(oTarget, sClassNames);
            }
            return applyDirect(oTarget, sClassNames, METHODS.REMOVE_STYLE_CLASS);
        },
        restart: function (oTarget, sClassName) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.restartOn === TYPE_FUNCTION) {
                return Ui5StyleAdapter.restartOn(oTarget, sClassName);
            }
            applyDirect(oTarget, sClassName, METHODS.REMOVE_STYLE_CLASS);
            forceReflow(oTarget);
            return applyDirect(oTarget, sClassName, METHODS.ADD_STYLE_CLASS);
        }
    };
});
