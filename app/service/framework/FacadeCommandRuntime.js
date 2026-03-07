sap.ui.define([], function () {
    "use strict";

    function executeRaw(oController, oFacade, sMethod, mInput, mCtx) {
        var fn = oFacade && oFacade[sMethod];
        if (typeof fn !== "function") {
            return Promise.resolve();
        }
        return Promise.resolve(oController.executeFacadeMethod(oFacade, sMethod, mInput || {}, mCtx || {}));
    }

    function executeWithContract(oController, oFacade, sMethod, mInput, mCtx, mContract) {
        var sCommand = mContract && typeof mContract.normalizeMethod === "function"
            ? mContract.normalizeMethod(sMethod)
            : sMethod;
        var oPayload = mContract && typeof mContract.normalizePayload === "function"
            ? mContract.normalizePayload(sCommand, mInput)
            : (mInput || {});
        return executeRaw(oController, oFacade, sCommand, oPayload, mCtx);
    }

    return {
        executeRaw: executeRaw,
        executeWithContract: executeWithContract
    };
});
