sap.ui.define([], function () {
    "use strict";

    function run(mOptions) {
        var oTarget = mOptions && mOptions.target;
        var sMethod = String((mOptions && mOptions.method) || "").trim();
        var fnInvoker = mOptions && mOptions.invoker;
        if (!oTarget || !sMethod || typeof fnInvoker !== "function") {
            return Promise.resolve();
        }
        if (typeof oTarget[sMethod] !== "function") {
            return Promise.resolve();
        }
        return Promise.resolve(fnInvoker(oTarget, sMethod));
    }

    function executeFacadeCommand(oController, oFacade, sMethod, oPayload, oCtx) {
        return run({
            target: oFacade,
            method: sMethod,
            invoker: function (oTarget, sResolvedMethod) {
                if (!oController || typeof oController.executeFacadeMethod !== "function") {
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
                if (!oController || typeof oController.applyUseCaseEffects !== "function") {
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
