sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxRuntimeFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionDispatcher",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (CtxRuntimeFactory, ActionDispatcher, RuntimeInput, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function resolveModel(oController, sName) {
        return oController && typeof oController.getModel === TYPE_FUNCTION ? oController.getModel(sName) : null;
    }

    function resolveRoute(oController) {
        return oController && typeof oController.getRouter === TYPE_FUNCTION ? oController.getRouter() : null;
    }

    function buildCtx(oController, mOptions) {
        return CtxRuntimeFactory.build(oController, mOptions || {});
    }

    function buildCommandContext(oController, oPayload, oCtx) {
        return {
            controller: oController,
            payload: RuntimeInput.asObject(oPayload),
            ctx: RuntimeInput.asObject(oCtx),
            model: function (sName) {
                return resolveModel(oController, sName);
            },
            stateModel: function () {
                return resolveModel(oController, "state");
            }
        };
    }

    function createDispatcher(mHandlers) {
        return new ActionDispatcher(mHandlers || {});
    }

    function executeCommand(oController, oTarget, sMethod, oPayload, oCtx) {
        var oContext = buildCommandContext(oController, oPayload, oCtx);
        if (!oTarget || typeof oTarget[sMethod] !== TYPE_FUNCTION) {
            return Promise.resolve(false);
        }
        return Promise.resolve(oTarget[sMethod].call(oTarget, oPayload || {}, oContext)).then(function () {
            return true;
        });
    }

    return Object.freeze({
        buildCtx: buildCtx,
        buildCommandContext: buildCommandContext,
        createDispatcher: createDispatcher,
        executeCommand: executeCommand,
        resolveModel: resolveModel,
        resolveRoute: resolveRoute
    });
});
