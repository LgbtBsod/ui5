sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (
    ModelContracts
) {
    "use strict";

    var I18N_MODEL = ModelContracts.MODELS.I18N;

    function normalizeActionPayload(ActionContract, sAction, mPayload) {
        var fnNormalize = ActionContract && ActionContract.normalizeActionPayload;

        if (typeof fnNormalize !== "function") {
            return mPayload || {};
        }

        return fnNormalize(sAction, mPayload);
    }

    function createBundleText(oComponent) {
        return function (sKey, aArgs) {
            var oI18nModel = oComponent && oComponent.getModel ? oComponent.getModel(I18N_MODEL) : null;
            var oBundle = oI18nModel && typeof oI18nModel.getResourceBundle === "function" ? oI18nModel.getResourceBundle() : null;
            if (!oBundle || typeof oBundle.getText !== "function") {
                return String(sKey || "");
            }
            try {
                return String(oBundle.getText(sKey, aArgs || []) || sKey || "");
            } catch (_bundleError) {
                return String(sKey || "");
            }
        };
    }

    function createApplyFacadeResult(mOptions) {
        var oComponent = mOptions.component;
        var oEffectApplier = mOptions.effectApplier;
        var oActionDispatcher = mOptions.actionDispatcher;
        var fnResolveBundleText = createBundleText(oComponent);

        return function (oResult) {
            oEffectApplier.applyEffects(oComponent, oResult && oResult.effects, {
                resolveTextKey: function (sKey) {
                    return fnResolveBundleText(sKey, []);
                },
                actionDispatcher: oActionDispatcher
            });
        };
    }

    function registerDefaultHandlers(mOptions) {
        var oActionDispatcher = mOptions.actionDispatcher;
        var oActionContract = mOptions.actionContract || {};
        var oDetailFacade = mOptions.detailFacade;
        var fnRunGuardedSave = mOptions.runGuardedSave;
        var fnBuildLatestCtx = mOptions.buildLatestCtx;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnGetCtx = mOptions.getCtx;
        var mActions = oActionContract.ACTIONS || {};

        if (!oActionDispatcher || typeof oActionDispatcher.register !== "function") {
            return;
        }

        if (mActions.DETAIL_RETRY_GUARDED_SAVE) {
            oActionDispatcher.register(mActions.DETAIL_RETRY_GUARDED_SAVE, function () {
                return fnRunGuardedSave();
            });
        }
        if (mActions.DETAIL_CONFIRM_ENTER_EDIT) {
            oActionDispatcher.register(mActions.DETAIL_CONFIRM_ENTER_EDIT, function (mPayload) {
                return oDetailFacade.enterEdit(
                    normalizeActionPayload(oActionContract, mActions.DETAIL_CONFIRM_ENTER_EDIT, mPayload || {}),
                    fnBuildLatestCtx()
                ).then(fnApplyFacadeResult);
            });
        }
        if (mActions.DETAIL_TAKEOVER_LOCK) {
            oActionDispatcher.register(mActions.DETAIL_TAKEOVER_LOCK, function (mPayload) {
                return oDetailFacade.confirmTakeover(
                    normalizeActionPayload(oActionContract, mActions.DETAIL_TAKEOVER_LOCK, mPayload || {}),
                    fnBuildLatestCtx()
                ).then(fnApplyFacadeResult);
            });
        }
        if (mActions.DETAIL_CANCEL_ENTER_EDIT) {
            oActionDispatcher.register(mActions.DETAIL_CANCEL_ENTER_EDIT, function (mPayload) {
                return oDetailFacade.cancelEnterEdit(
                    normalizeActionPayload(oActionContract, mActions.DETAIL_CANCEL_ENTER_EDIT, mPayload || {}),
                    fnGetCtx()
                ).then(fnApplyFacadeResult);
            });
        }
    }

    return {
        createApplyFacadeResult: createApplyFacadeResult,
        createBundleText: createBundleText,
        registerDefaultHandlers: registerDefaultHandlers
    };
});
