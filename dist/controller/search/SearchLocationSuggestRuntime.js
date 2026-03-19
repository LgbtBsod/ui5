sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (JsRuntime, SchedulingRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function bindLocationSuggest(oController) {
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var oLocationControl;
        if (!oSmartFilterBar || typeof oSmartFilterBar.getControlByKey !== TYPE_FUNCTION) {
            return;
        }
        oLocationControl = oSmartFilterBar.getControlByKey("LocationKey");
        if (!oLocationControl || oLocationControl.data("locationSuggestBound")) {
            return;
        }
        if (typeof oLocationControl.setShowSuggestion === TYPE_FUNCTION) {
            oLocationControl.setShowSuggestion(true);
        }
        if (typeof oLocationControl.attachSuggest === TYPE_FUNCTION) {
            oLocationControl.attachSuggest(oController.onLocationKeySuggest, oController);
        }
        if (typeof oLocationControl.attachSuggestionItemSelected === TYPE_FUNCTION) {
            oLocationControl.attachSuggestionItemSelected(oController.onLocationKeySuggestionSelected, oController);
        }
        oLocationControl.data("locationSuggestBound", true);
    }

    function updateLocationSuggestions(oControl, aItems, Item) {
        if (!oControl || typeof oControl.destroySuggestionItems !== TYPE_FUNCTION || typeof oControl.addSuggestionItem !== TYPE_FUNCTION) {
            return;
        }
        oControl.destroySuggestionItems();
        (aItems || []).slice(0, 24).forEach(function (oItem) {
            var sCode = String((oItem && (oItem.location_code || oItem.location_id)) || "").trim();
            var sName = String((oItem && oItem.location_name) || "").trim();
            if (!sCode && !sName) {
                return;
            }
            oControl.addSuggestionItem(new Item({
                key: sCode,
                text: sCode,
                additionalText: sName
            }));
        });
    }

    function runLocationSuggest(oController, oEvent, Item) {
        var sValue = String(oEvent && oEvent.getParameter && (oEvent.getParameter("suggestValue") || oEvent.getParameter("value")) || "").trim();
        var oControl = oEvent && oEvent.getSource && oEvent.getSource();
        var oCtx = oController._ctx && oController._ctx();
        var oLookup = oCtx && oCtx.locationLookup;
        var sNeedle = sValue.toLowerCase();
        oController._iLocationSuggestTimer = SchedulingRuntime.clearTimer(oController._iLocationSuggestTimer);
        oController._iLocationSuggestTimer = SchedulingRuntime.restartTimer(0, function () {
            var iRequestVersion;
            oController._iLocationSuggestTimer = null;
            if (!oControl) {
                return;
            }
            iRequestVersion = Number(oController._iLocationSuggestRequestVersion || 0) + 1;
            oController._iLocationSuggestRequestVersion = iRequestVersion;
            if (sNeedle && Array.isArray(oController._aLocationSuggestCache) && oController._aLocationSuggestCache.length && oController._sLocationSuggestNeedle && sNeedle.indexOf(oController._sLocationSuggestNeedle) === 0) {
                updateLocationSuggestions(oControl, oController._aLocationSuggestCache.filter(function (oItem) {
                    var sCode = String((oItem && (oItem.location_code || oItem.location_id)) || "").toLowerCase();
                    var sName = String((oItem && oItem.location_name) || "").toLowerCase();
                    return sCode.indexOf(sNeedle) >= 0 || sName.indexOf(sNeedle) >= 0;
                }), Item);
                return;
            }
            if (!oLookup || typeof oLookup.search !== TYPE_FUNCTION) {
                updateLocationSuggestions(oControl, [], Item);
                return;
            }
            Promise.resolve(oLookup.search({ query: sValue, limit: 50 }))
                .then(function (oFound) {
                    var aItems;
                    if (oController._iLocationSuggestRequestVersion !== iRequestVersion) {
                        return;
                    }
                    aItems = (oFound && oFound.items) || [];
                    oController._aLocationSuggestCache = aItems;
                    oController._sLocationSuggestNeedle = sNeedle;
                    updateLocationSuggestions(oControl, aItems, Item);
                })
                .catch(function () {
                    if (oController._iLocationSuggestRequestVersion !== iRequestVersion) {
                        return;
                    }
                    updateLocationSuggestions(oControl, [], Item);
                });
        }, 180);
    }

    function applyLocationSuggestionSelection(oEvent) {
        var oSelected = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        var oControl = oEvent && oEvent.getSource && oEvent.getSource();
        if (!oSelected || !oControl || typeof oControl.setValue !== TYPE_FUNCTION) {
            return;
        }
        oControl.setValue(oSelected.getKey ? oSelected.getKey() : oSelected.getText());
    }

    return {
        bindLocationSuggest: bindLocationSuggest,
        runLocationSuggest: runLocationSuggest,
        applyLocationSuggestionSelection: applyLocationSuggestionSelection
    };
});
