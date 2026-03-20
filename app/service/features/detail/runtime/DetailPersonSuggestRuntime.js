sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceConstants"
], function (OperationSourceContracts) {
    "use strict";

    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;

    return {
        onPersonInputChange: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sTarget = mHooks.personTargetFromSource(oSource);
            var sValue;
            if (!mHooks.isEditMode()) {
                return;
            }
            sValue = String((oEvent && oEvent.getParameter && oEvent.getParameter("value")) || "");
            if (mHooks.consumeSuggestionSelection(sTarget, sValue)) {
                return;
            }
            return mHooks.personSuggest({
                intent: DETAIL_SOURCES.MANUAL_CHANGE,
                value: sValue,
                target: sTarget
            });
        },
        onPersonSuggest: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            if (!mHooks.isEditMode()) {
                return;
            }
            return mHooks.personSuggest({
                term: oEvent.getParameter("suggestValue"),
                target: mHooks.personTargetFromSource(oSource)
            });
        },
        onPersonSuggestionSelected: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
            var sTarget = mHooks.personTargetFromSource(oSource);
            var sSelectedValue = "";
            if (!mHooks.isEditMode()) {
                return;
            }
            if (oSelectedItem && typeof oSelectedItem.getText === "function") {
                sSelectedValue = String(oSelectedItem.getText() || "");
            }
            mHooks.rememberSuggestionSelection(sTarget, sSelectedValue);
            return mHooks.personSuggest({
                intent: DETAIL_SOURCES.SELECTED,
                item: oSelectedItem,
                target: sTarget
            });
        }
    };
});
