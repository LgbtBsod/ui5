sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function run(mOptions) {
        var oTarget = mOptions && mOptions.target;
        var sMethod = String((mOptions && mOptions.method) || "").trim();
        var fnInvoker = mOptions && mOptions.invoker;
        if (!oTarget || !sMethod || typeof fnInvoker !== TYPE_FUNCTION) {
            return Promise.resolve();
        }
        if (typeof oTarget[sMethod] !== TYPE_FUNCTION) {
            return Promise.resolve();
        }
        return Promise.resolve(fnInvoker(oTarget, sMethod));
    }

    function executeFacadeCommand(oController, oFacade, sMethod, oPayload, oCtx) {
        return run({
            target: oFacade,
            method: sMethod,
            invoker: function (oTarget, sResolvedMethod) {
                if (!oController || typeof oController.executeFacadeMethod !== TYPE_FUNCTION) {
                    return Promise.resolve();
                }
                return oController.executeFacadeMethod(oTarget, sResolvedMethod, oPayload || {}, oCtx || {});
            }
        });
    }

    function executeUseCaseCommand(oController, oService, sMethod, oPayload, oCtx) {
        return run({
            target: oService,
            method: sMethod,
            invoker: function (oTarget, sResolvedMethod) {
                if (!oController || typeof oController.applyUseCaseEffects !== TYPE_FUNCTION) {
                    return Promise.resolve();
                }
                return Promise.resolve(oTarget[sResolvedMethod].call(oTarget, oPayload || {}, oCtx || {})).then(function (oResult) {
                    return oController.applyUseCaseEffects(oResult);
                });
            }
        });
    }

    return Object.freeze({
        executeFacadeCommand: executeFacadeCommand,
        executeUseCaseCommand: executeUseCaseCommand
    });
});
