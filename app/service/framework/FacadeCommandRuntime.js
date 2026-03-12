sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandPayloadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandContextRuntime"
], function (FacadeCommandContracts, FacadeCommandPayloadRuntime, FacadeCommandContextRuntime) {
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
            : FacadeCommandContextRuntime.buildDefaultCtx;
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
            buildCtx: FacadeCommandContextRuntime.buildDefaultCtx,
            normalizeMethod: function (vMethod) {
                return FacadeCommandContracts.normalizeKnownMethod(vMethod, FacadeCommandContracts.DETAIL_METHODS);
            },
            normalizePayload: FacadeCommandPayloadRuntime.normalizePayload
        });
    }

    function executeSearch(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: FacadeCommandContextRuntime.buildSearchCtx,
            normalizeMethod: function (vMethod) {
                return FacadeCommandContracts.normalizeKnownMethod(vMethod, FacadeCommandContracts.SEARCH_METHODS);
            },
            normalizePayload: FacadeCommandPayloadRuntime.normalizePayload
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
