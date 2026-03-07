sap.ui.define([
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/framework/FacadeCommandContract"
], function (ControllerCtxRuntime, FacadeCommandContract) {
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

    function executeNamed(oController, oFacade, sMethod, mInput, mProfile) {
        var oProfile = mProfile || {};
        var fnBuildCtx = typeof oProfile.buildCtx === "function"
            ? oProfile.buildCtx
            : ControllerCtxRuntime.buildDefault;
        return executeWithContract(
            oController,
            oFacade,
            sMethod,
            mInput || {},
            fnBuildCtx(oController),
            {
                normalizeMethod: oProfile.normalizeMethod,
                normalizePayload: oProfile.normalizePayload
            }
        );
    }

    function executeDetail(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: ControllerCtxRuntime.buildDefault,
            normalizeMethod: FacadeCommandContract.normalizeDetailMethod,
            normalizePayload: FacadeCommandContract.normalizeDetailPayload
        });
    }

    function executeSearch(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: ControllerCtxRuntime.buildSearch,
            normalizeMethod: FacadeCommandContract.normalizeSearchMethod,
            normalizePayload: FacadeCommandContract.normalizeSearchPayload
        });
    }

    return {
        executeRaw: executeRaw,
        executeWithContract: executeWithContract,
        executeNamed: executeNamed,
        executeDetail: executeDetail,
        executeSearch: executeSearch
    };
});
