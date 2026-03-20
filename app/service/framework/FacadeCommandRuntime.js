sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer"
], function (
    FacadeCommandContracts,
    ControllerCommandRuntime,
    ControllerCommandContextRuntime,
    RuntimePayloadNormalizer
) {
    "use strict";

    function normalizePayload(vCommandOrPayload, oPayload) {
        return RuntimePayloadNormalizer.normalize(
            arguments.length > 1 ? oPayload : vCommandOrPayload
        );
    }

    function executeRaw(oController, oFacade, sMethod, mInput, mCtx) {
        return ControllerCommandRuntime.executeFacadeCommand(
            oController,
            oFacade,
            sMethod,
            mInput || {},
            mCtx || {}
        );
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
            : ControllerCommandContextRuntime.buildDefaultCtx;
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
            buildCtx: ControllerCommandContextRuntime.buildDefaultCtx,
            normalizeMethod: function (vMethod) {
                return FacadeCommandContracts.normalizeKnownMethod(vMethod, FacadeCommandContracts.DETAIL_METHODS);
            },
            normalizePayload: normalizePayload
        });
    }

    function executeSearch(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: ControllerCommandContextRuntime.buildSearchCtx,
            normalizeMethod: function (vMethod) {
                return FacadeCommandContracts.normalizeKnownMethod(vMethod, FacadeCommandContracts.SEARCH_METHODS);
            },
            normalizePayload: normalizePayload
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
