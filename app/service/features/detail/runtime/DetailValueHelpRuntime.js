sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailNumberValueHelpRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailPersonSuggestRuntime"
], function (OperationSourceContracts, DetailFieldContracts, DetailNumberValueHelpRuntime, DetailPersonSuggestRuntime) {
    "use strict";

    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;
    var VALUE_HELP_DIALOGS = DetailFieldContracts.VALUE_HELP_DIALOGS;
    var VIEW_PATHS = DetailFieldContracts.VIEW_PATHS;

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
        onBarriersNumberChange: DetailNumberValueHelpRuntime.onBarriersNumberChange,
        onChecksNumberChange: DetailNumberValueHelpRuntime.onChecksNumberChange,
        onOpenBarriersNumberValueHelp: DetailNumberValueHelpRuntime.onOpenBarriersNumberValueHelp,
        onOpenChecksNumberValueHelp: DetailNumberValueHelpRuntime.onOpenChecksNumberValueHelp,
        onLocationTreeSelectionChange: function (_oController, oEvent, mHooks) {
            return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.TREE_SELECTION, event: oEvent });
        },
        onLocationValueHelpSearch: function (_oController, oEvent, mHooks) {
            var sValue = oEvent.getParameter("newValue");
            mHooks.clearSearchTimer();
            mHooks.restartSearchTimer(function () {
                Promise.resolve(mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                    return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.SEARCH, value: sValue });
                })).then(function () {
                    mHooks.scheduleTableSync();
                }).catch(function () {
                    return;
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
        onPersonInputChange: DetailPersonSuggestRuntime.onPersonInputChange,
        onPersonSuggest: DetailPersonSuggestRuntime.onPersonSuggest,
        onPersonSuggestionSelected: DetailPersonSuggestRuntime.onPersonSuggestionSelected,
        onLpcChange: DetailNumberValueHelpRuntime.onLpcChange,
        onProfessionChange: DetailNumberValueHelpRuntime.onProfessionChange
    };
});
