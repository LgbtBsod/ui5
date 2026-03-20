sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardContracts"
], function (FeedbackBannerState, ModelStateRuntime, ComponentSaveGuardContracts) {
    "use strict";

    var BANNER_LEVEL = ComponentSaveGuardContracts.BANNER_LEVEL;
    var BANNER_TEXT_KEY = ComponentSaveGuardContracts.BANNER_TEXT_KEY;

    function bannerPath(sId) {
        return "/ui/feedback/banner/" + String(sId || "global");
    }

    function setBanner(oStateModel, sId, mInput, mOptions) {
        return ModelStateRuntime.writeOnModel(
            oStateModel,
            bannerPath(sId),
            FeedbackBannerState.create(mInput || {}, mOptions || {})
        );
    }

    function getBanner(oStateModel, sId) {
        return ModelStateRuntime.readOnModel(oStateModel, bannerPath(sId), {}) || {};
    }

    function getBannerProperty(oStateModel, sId, sProperty) {
        var oBanner = getBanner(oStateModel, sId);
        return oBanner ? oBanner[sProperty] : undefined;
    }

    function createBannerInput(mInput) {
        return Object.assign({
            severity: "info",
            scope: "global",
            textKey: "",
            textArgs: [],
            details: "",
            correlationId: "",
            retryAction: "",
            retryTextKey: ""
        }, mInput || {});
    }

    function createRetryBannerInput(sSeverity, sTextKey, mOptions) {
        return createBannerInput(Object.assign({
            severity: sSeverity,
            textKey: sTextKey
        }, mOptions || {}));
    }

    function createNetworkRetryBannerInput(sRetryAction, sRetryTextKey, sDetails) {
        return createRetryBannerInput(BANNER_LEVEL.WARNING, BANNER_TEXT_KEY.NETWORK_UNAVAILABLE, {
            details: String(sDetails || ""),
            retryAction: String(sRetryAction || ""),
            retryTextKey: String(sRetryTextKey || "")
        });
    }

    function setGlobalMessage(oStateModel, sSeverity, sText) {
        return setBanner(oStateModel, "global", {
            visible: true,
            scope: "global",
            severity: sSeverity,
            text: sText
        });
    }

    function resolveBannerScope(oEffect) {
        var oPayload = (oEffect && oEffect.payload) || {};
        return FeedbackBannerState.normalizeScope(
            (oEffect && oEffect.scope)
            || oPayload.scope
            || "route"
        );
    }

    function resolveBannerId(oEffect) {
        if (oEffect && oEffect.id) {
            return oEffect.id;
        }
        return resolveBannerScope(oEffect) === "global" ? "global" : "route";
    }

    function fromEffect(oEffect, sText) {
        var oPayload = (oEffect && oEffect.payload) || {};
        return Object.assign({}, oPayload, {
            visible: oEffect && Object.prototype.hasOwnProperty.call(oEffect, "visible") ? !!oEffect.visible : true,
            scope: resolveBannerScope(oEffect),
            text: sText
        });
    }

    function applyEffect(oStateModel, oEffect, sText) {
        return setBanner(
            oStateModel,
            resolveBannerId(oEffect),
            fromEffect(oEffect, sText)
        );
    }

    function clearFromEffect(oStateModel, oEffect) {
        return clearBanner(oStateModel, resolveBannerId(oEffect));
    }

    function clearBanner(oStateModel, sId) {
        return ModelStateRuntime.writeOnModel(oStateModel, bannerPath(sId), FeedbackBannerState.empty());
    }

    return {
        bannerPath: bannerPath,
        setBanner: setBanner,
        getBanner: getBanner,
        getBannerProperty: getBannerProperty,
        createBannerInput: createBannerInput,
        createRetryBannerInput: createRetryBannerInput,
        createNetworkRetryBannerInput: createNetworkRetryBannerInput,
        setGlobalMessage: setGlobalMessage,
        resolveBannerId: resolveBannerId,
        fromEffect: fromEffect,
        applyEffect: applyEffect,
        clearFromEffect: clearFromEffect,
        clearBanner: clearBanner
    };
});
