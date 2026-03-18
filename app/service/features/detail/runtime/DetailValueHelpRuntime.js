sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts"
], function (ModelStateRuntime, ModelContracts, OperationSourceContracts, DetailFieldContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;
    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;
    var AUTOSAVE_FIELDS = DetailFieldContracts.AUTOSAVE_FIELDS;
    var TEXT_PATHS = DetailFieldContracts.TEXT_PATHS;
    var VALUE_HELP_DIALOGS = DetailFieldContracts.VALUE_HELP_DIALOGS;
    var VIEW_PATHS = DetailFieldContracts.VIEW_PATHS;

    function resolveSelectChangeValue(oEvent) {
        return String((oEvent && oEvent.getParameter && oEvent.getParameter("value")) || "").trim();
    }

    function resolveSelectChangeText(oEvent) {
        var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        if (oSelectedItem && typeof oSelectedItem.getText === "function") {
            return String(oSelectedItem.getText() || "").trim();
        }
        return "";
    }

    function autosaveSelectChange(oController, oEvent, sTextPath, sFieldKey, mHooks) {
        var sValue = resolveSelectChangeValue(oEvent);
        ModelStateRuntime.write(oController, SELECTED_MODEL, sTextPath, resolveSelectChangeText(oEvent));
        mHooks.autosave({
            field: sFieldKey,
            value: sValue
        });
    }

    return {
        closeLocationValueHelp: function (_oController, mHooks) {
            mHooks.clearSearchTimer();
            return Promise.resolve(mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.CLOSE })).finally(function () {
                mHooks.setViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, false);
            });
        },
        confirmLocationValueHelp: function (_oController, mHooks) {
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.CONFIRM });
            });
        },
        onBarriersNumberChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.BARRIERS_NUMBER, AUTOSAVE_FIELDS.BARRIERS_NUMBER, mHooks);
        },
        onChecksNumberChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.CHECKS_NUMBER, AUTOSAVE_FIELDS.CHECKS_NUMBER, mHooks);
        },
        onLocationTreeSelectionChange: function (_oController, oEvent, mHooks) {
            return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.TREE_SELECTION, event: oEvent });
        },
        onLocationValueHelpSearch: function (_oController, oEvent, mHooks) {
            var sValue = oEvent.getParameter("newValue");
            mHooks.clearSearchTimer();
            mHooks.restartSearchTimer(function () {
                mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                    return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.SEARCH, value: sValue });
                }).then(function () {
                    mHooks.scheduleTableSync();
                });
            }, 180);
        },
        onLocationValueHelpSearchSubmit: function (_oController, oEvent, mHooks) {
            var sQuery = oEvent.getParameter("query");
            mHooks.clearSearchTimer();
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.SEARCH, value: sQuery });
            }).then(function () {
                mHooks.scheduleTableSync();
            });
        },
        onLpcChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.LPC, AUTOSAVE_FIELDS.LPC_KEY, mHooks);
        },
        onOpenLocationValueHelp: function (_oController, oEvent, mHooks) {
            mHooks.clearSearchTimer();
            mHooks.rememberDialogReturnFocus(VALUE_HELP_DIALOGS.LOCATION, oEvent && oEvent.getSource && oEvent.getSource());
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.OPEN });
            }).then(function (oResult) {
                mHooks.scheduleTableSync();
                return oResult;
            });
        },
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
        },
        onProfessionChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.PROFESSION, AUTOSAVE_FIELDS.PROF_KEY, mHooks);
        }
    };
});
