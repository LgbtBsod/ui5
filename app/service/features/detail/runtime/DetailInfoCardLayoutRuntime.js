sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReadSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutPersonalizationRuntime"
], function (BindingContextReadSupport, ControllerViewStateRuntime, ModelStateRuntime, FocusRuntime, LayoutPersonalizationRuntime) {
    "use strict";

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
        ModelStateRuntime.write(oController, "layout", "/personalization/infoCardLayout", aLayout);
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
            return BindingContextReadSupport.read(oContext, "key", "") === sKey;
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
        var aLayout = ModelStateRuntime.read(oController, "layout", "/personalization/infoCardLayout", []);
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
