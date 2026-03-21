sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReader",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutPersonalizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (BindingContextReader, ControllerViewStateRuntime, ModelStateRuntime, FocusRuntime, LayoutPersonalizationRuntime, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;

    function cloneCards(aCards) {
        return (Array.isArray(aCards) ? aCards : []).map(function (oCard, iIndex) {
            return Object.assign({ order: iIndex }, oCard);
        });
    }

    function normalizeCards(aCards) {
        return cloneCards(aCards).map(function (oCard, iIndex) {
            oCard.order = iIndex;
            return oCard;
        });
    }

    function persistLayout(oController, aCards) {
        var aLayout = normalizeCards(aCards).map(function (oCard, iIndex) {
            return { key: oCard.key, pinned: !!oCard.pinned, order: iIndex };
        });
        ModelStateRuntime.write(oController, MODELS.SHELL, MODEL_PATHS.SHELL_PERSONALIZATION_INFO_CARD_LAYOUT, aLayout);
        LayoutPersonalizationRuntime.writeInfoCardLayout(aLayout);
        return aLayout;
    }

    function findCardIndex(aCards, sKey) {
        return aCards.findIndex(function (oCard) { return oCard.key === sKey; });
    }

    function focusCardByKey(oController, sKey) {
        var oGrid = oController.byId && oController.byId("infoCardGrid");
        var oItem = oGrid && oGrid.getItems && oGrid.getItems().find(function (oEntry) {
            var oContext = oEntry.getBindingContext && oEntry.getBindingContext("view");
        return BindingContextReader.read(oContext, "key", "") === sKey;
        });
        if (oItem) {
            FocusRuntime.focusSoon(oItem);
        }
    }

    function writeCards(oController, aCards, sFocusKey) {
        var aNormalized = normalizeCards(aCards);
        ControllerViewStateRuntime.set(oController, "/infoCards", aNormalized);
        persistLayout(oController, aNormalized);
        if (sFocusKey) {
            focusCardByKey(oController, sFocusKey);
        }
    }

    function resolveCards(oController, aBaseCards) {
        var aLayout = ModelStateRuntime.read(oController, MODELS.SHELL, MODEL_PATHS.SHELL_PERSONALIZATION_INFO_CARD_LAYOUT, []);
        var mLayoutByKey = {};
        var aOrderedKeys = [];
        cloneCards(aLayout).forEach(function (oEntry, iIndex) {
            mLayoutByKey[oEntry.key] = { pinned: !!oEntry.pinned, order: Number.isFinite(oEntry.order) ? oEntry.order : iIndex };
            aOrderedKeys.push(oEntry.key);
        });
        return normalizeCards(
            cloneCards(aBaseCards).sort(function (oLeft, oRight) {
                var oLeftLayout = mLayoutByKey[oLeft.key];
                var oRightLayout = mLayoutByKey[oRight.key];
                var iLeft = oLeftLayout ? oLeftLayout.order : (aOrderedKeys.length + oLeft.order);
                var iRight = oRightLayout ? oRightLayout.order : (aOrderedKeys.length + oRight.order);
                return iLeft - iRight;
            }).map(function (oCard) {
                var oLayout = mLayoutByKey[oCard.key];
                return Object.assign({}, oCard, oLayout ? { pinned: oLayout.pinned } : null);
            })
        );
    }

    function moveCard(oController, sKey, iDelta) {
        var aCards = cloneCards(ControllerViewStateRuntime.get(oController, "/infoCards", []));
        var iFrom = findCardIndex(aCards, sKey);
        var iTo;
        var oCard;
        if (iFrom < 0) {
            return false;
        }
        iTo = Math.max(0, Math.min(aCards.length - 1, iFrom + iDelta));
        if (iFrom === iTo) {
            return false;
        }
        oCard = aCards[iFrom];
        aCards.splice(iFrom, 1);
        aCards.splice(iTo, 0, oCard);
        writeCards(oController, aCards, sKey);
        return true;
    }

    function togglePin(oController, sKey) {
        var aCards = cloneCards(ControllerViewStateRuntime.get(oController, "/infoCards", []));
        var iIndex = findCardIndex(aCards, sKey);
        if (iIndex < 0) {
            return false;
        }
        aCards[iIndex].pinned = !aCards[iIndex].pinned;
        writeCards(oController, aCards, sKey);
        return true;
    }

    return {
        resolveCards: resolveCards,
        writeCards: writeCards,
        moveCard: moveCard,
        togglePin: togglePin,
        focusCardByKey: focusCardByKey
    };
});
