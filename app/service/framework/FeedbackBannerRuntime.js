sap.ui.define([
    "checklist/app/service/framework/FeedbackBannerState"
], function (FeedbackBannerState) {
    "use strict";

    function bannerPath(sId) {
        return "/ui/feedback/banner/" + String(sId || "global");
    }

    function setBanner(oStateModel, sId, mInput, mOptions) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return false;
        }
        oStateModel.setProperty(bannerPath(sId), FeedbackBannerState.create(mInput || {}, mOptions || {}));
        return true;
    }

    function getBanner(oStateModel, sId) {
        if (!oStateModel || typeof oStateModel.getProperty !== "function") {
            return {};
        }
        return oStateModel.getProperty(bannerPath(sId)) || {};
    }

    function getBannerProperty(oStateModel, sId, sProperty) {
        var oBanner = getBanner(oStateModel, sId);
        return oBanner ? oBanner[sProperty] : undefined;
    }

    function createBannerInput(mInput) {
        return Object.assign({
            severity: "info",
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
        return createRetryBannerInput("warning", "networkUnavailable", {
            details: String(sDetails || ""),
            retryAction: String(sRetryAction || ""),
            retryTextKey: String(sRetryTextKey || "")
        });
    }

    function setGlobalMessage(oStateModel, sSeverity, sText) {
        return setBanner(oStateModel, "global", {
            visible: true,
            severity: sSeverity,
            text: sText
        });
    }

    function resolveBannerId(oEffect) {
        return (oEffect && oEffect.id) || "global";
    }

    function fromEffect(oEffect, sText) {
        var oPayload = (oEffect && oEffect.payload) || {};
        return Object.assign({}, oPayload, {
            visible: oEffect && Object.prototype.hasOwnProperty.call(oEffect, "visible") ? !!oEffect.visible : true,
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
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return false;
        }
        oStateModel.setProperty(bannerPath(sId), FeedbackBannerState.empty());
        return true;
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
