sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function splitClassNames(sClassNames) {
        return String(sClassNames || "").split(/\s+/).map(function (sName) {
            return sName.trim();
        }).filter(Boolean);
    }

    function resolveTarget(oController, sTarget) {
        var mResolvers = {
            view: function () {
                return oController && oController.getView ? oController.getView() : null;
            },
            component: function () {
                return oController && oController.getOwnerComponent ? oController.getOwnerComponent() : null;
            }
        };
        var fnResolver = mResolvers[String(sTarget || "")];
        var oResolved = typeof fnResolver === TYPE_FUNCTION ? fnResolver() : null;
        return oResolved || (oController && oController.getView && oController.getView()) || null;
    }

    function applyClassNameToTarget(oTarget, sClassName, sAction) {
        var mHandlers;
        var fnHandler;
        if (!oTarget || !sClassName) {
            return;
        }
        mHandlers = {
            enable: function () {
                if (oTarget[METHODS.ADD_STYLE_CLASS]) {
                    oTarget[METHODS.ADD_STYLE_CLASS](sClassName);
                }
            },
            disable: function () {
                if (oTarget[METHODS.REMOVE_STYLE_CLASS]) {
                    oTarget[METHODS.REMOVE_STYLE_CLASS](sClassName);
                }
            }
        };
        fnHandler = mHandlers[String(sAction || "")];
        if (typeof fnHandler === TYPE_FUNCTION) {
            fnHandler();
        }
    }

    function applyClassNamesToTarget(oTarget, sClassNames, sAction) {
        splitClassNames(sClassNames).forEach(function (sClassName) {
            applyClassNameToTarget(oTarget, sClassName, sAction);
        });
        return oTarget;
    }

    function restartClassNameOnTarget(oTarget, sClassName) {
        var oDomRef;
        applyClassNamesToTarget(oTarget, sClassName, "disable");
        oDomRef = oTarget && oTarget[METHODS.GET_DOM_REF] && oTarget[METHODS.GET_DOM_REF]();
        if (oDomRef) {
            void oDomRef.offsetWidth;
        }
        applyClassNamesToTarget(oTarget, sClassName, "enable");
        return oTarget;
    }

    function applyClassName(oController, sClassName, sAction, sTarget) {
        return applyClassNamesToTarget(resolveTarget(oController, sTarget), sClassName, sAction);
    }

    return {
        enable: function (oController, sClassName, sTarget) {
            applyClassName(oController, sClassName, "enable", sTarget);
        },
        disable: function (oController, sClassName, sTarget) {
            applyClassName(oController, sClassName, "disable", sTarget);
        },
        enableOn: function (oTarget, sClassNames) {
            return applyClassNamesToTarget(oTarget, sClassNames, "enable");
        },
        disableOn: function (oTarget, sClassNames) {
            return applyClassNamesToTarget(oTarget, sClassNames, "disable");
        },
        restartOn: function (oTarget, sClassName) {
            return restartClassNameOnTarget(oTarget, sClassName);
        }
    };
});
