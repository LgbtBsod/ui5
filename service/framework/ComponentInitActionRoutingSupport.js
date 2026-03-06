sap.ui.define([], function () {
    "use strict";

    function buildActionValidators(ActionContract) {
        var mValidators = {};
        var mActions = (ActionContract && ActionContract.ACTIONS) || {};
        var fnNormalize = ActionContract && ActionContract.normalizeActionPayload;

        if (typeof fnNormalize !== "function") {
            return mValidators;
        }

        [
            mActions.DETAIL_RETRY_GUARDED_SAVE,
            mActions.DETAIL_TAKEOVER_LOCK,
            mActions.DETAIL_CANCEL_ENTER_EDIT
        ].forEach(function (sAction) {
            if (!sAction) {
                return;
            }
            mValidators[sAction] = function (mPayload) {
                return fnNormalize(sAction, mPayload);
            };
        });

        return mValidators;
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
        if (mActions.DETAIL_TAKEOVER_LOCK) {
            oActionDispatcher.register(mActions.DETAIL_TAKEOVER_LOCK, function (mPayload) {
                return oDetailFacade.confirmTakeover(mPayload || {}, fnBuildLatestCtx()).then(fnApplyFacadeResult);
            });
        }
        if (mActions.DETAIL_CANCEL_ENTER_EDIT) {
            oActionDispatcher.register(mActions.DETAIL_CANCEL_ENTER_EDIT, function (mPayload) {
                return oDetailFacade.cancelEnterEdit(mPayload || {}, fnGetCtx()).then(fnApplyFacadeResult);
            });
        }
    }

    return {
        buildActionValidators: buildActionValidators,
        registerDefaultHandlers: registerDefaultHandlers
    };
});
