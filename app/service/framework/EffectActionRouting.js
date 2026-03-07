sap.ui.define([
    "sap_ui5/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    function resolveActionDispatcher(oController, oOptions) {
        if (oOptions && oOptions.actionDispatcher) {
            return oOptions.actionDispatcher;
        }
        var oOwner = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        return oOwner && oOwner._actionDispatcher;
    }

    function normalizeActionName(vAction) {
        return RuntimeInput.asString(vAction).trim();
    }

    function normalizeEffectVerb(vAction) {
        return RuntimeInput.asString(vAction).trim().toLowerCase();
    }

    function resolveActionName(oEffect, oOverrides) {
        var oPayload = RuntimeInput.asObject(oEffect && oEffect.payload);
        var oMap = RuntimeInput.asObject(oOverrides);
        var sAction = normalizeActionName(
            oMap.actionName ||
            (oEffect && oEffect.actionName) ||
            oPayload.actionName ||
            oPayload.action
        );
        return sAction;
    }

    function resolveActionPayload(oEffect, oOverrides) {
        var oPayload = RuntimeInput.asObject(oEffect && oEffect.payload);
        var oMap = RuntimeInput.asObject(oOverrides);
        var oDirectPayload = RuntimeInput.asObject(
            oMap.actionPayload ||
            oPayload.actionPayload ||
            oPayload.payload
        );
        if (Object.keys(oDirectPayload).length) {
            return oDirectPayload;
        }
        // Fallback for dispatch effects where payload is flat and action meta is in the same object.
        var oSanitized = Object.assign({}, oPayload);
        delete oSanitized.action;
        delete oSanitized.actionName;
        delete oSanitized.actionPayload;
        delete oSanitized.payload;
        return RuntimeInput.asObject(oSanitized);
    }

    function dispatchByName(oController, oOptions, vActionName, mPayload) {
        var sActionName = normalizeActionName(vActionName);
        var oDispatcher = resolveActionDispatcher(oController, oOptions);
        if (!sActionName || !oDispatcher || typeof oDispatcher.dispatch !== "function") {
            return Promise.resolve(false);
        }
        return Promise.resolve(oDispatcher.dispatch(sActionName, RuntimeInput.asObject(mPayload))).then(function (vHandled) {
            return vHandled !== false;
        });
    }

    function dispatchEffectAction(oController, oOptions, oEffect, oOverrides) {
        var sActionName = resolveActionName(oEffect, oOverrides);
        var oActionPayload = resolveActionPayload(oEffect, oOverrides);
        return dispatchByName(oController, oOptions, sActionName, oActionPayload);
    }

    return {
        resolveActionDispatcher: resolveActionDispatcher,
        normalizeActionName: normalizeActionName,
        normalizeEffectVerb: normalizeEffectVerb,
        resolveActionName: resolveActionName,
        resolveActionPayload: resolveActionPayload,
        dispatchByName: dispatchByName,
        dispatchEffectAction: dispatchEffectAction
    };
});
