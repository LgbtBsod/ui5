sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (SearchToolbarContracts, ModelStateRuntime, OperationSourceContracts, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var TOKENS = ModelContracts.TOKENS;

    function shouldRebindSearch(oController, ControllerViewStateRuntime) {
        return !!(ControllerViewStateRuntime.get(oController, "/hasSearched")
            && ControllerViewStateRuntime.get(oController, "/smartTableReady"));
    }

    function buildSortSettingsFromEvent(oEvent) {
        var oSortItem = oEvent && oEvent.getParameter && oEvent.getParameter("sortItem");
        return {
            sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
            sortDescending: !!(oEvent && oEvent.getParameter && oEvent.getParameter("sortDescending"))
        };
    }

    function buildGroupSettingsFromEvent(oEvent) {
        var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
        return {
            groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
            groupDescending: !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"))
        };
    }

    function applySearchSortSettings(oController, mSettings, mDeps) {
        var sSortKey = String((mSettings && mSettings.sortKey) || "").trim() || TOKENS.DATE_CHECK;
        var bSortDescending = !!(mSettings && mSettings.sortDescending);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, sSortKey);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, bSortDescending);
        if (shouldRebindSearch(oController, mDeps.ControllerViewStateRuntime)) {
            return mDeps.SearchCommandPolicy.rebind(oController, { source: OperationSourceContracts.SEARCH.SEARCH_SORT_SETTINGS });
        }
        return Promise.resolve({
            source: OperationSourceContracts.SEARCH.SEARCH_SORT_SETTINGS,
            skipped: true
        });
    }

    function applySearchGroupSettings(oController, mSettings, mDeps) {
        var sGroupKey = String((mSettings && mSettings.groupKey) || "").trim();
        var bGroupDescending = !!(mSettings && mSettings.groupDescending);
        if (sGroupKey === TOKENS.GROUP_NONE) {
            sGroupKey = "";
        }
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, sGroupKey);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, bGroupDescending);
        if (shouldRebindSearch(oController, mDeps.ControllerViewStateRuntime)) {
            return mDeps.SearchCommandPolicy.rebind(oController, { source: OperationSourceContracts.SEARCH.SEARCH_GROUP_SETTINGS });
        }
        return Promise.resolve({
            source: OperationSourceContracts.SEARCH.SEARCH_GROUP_SETTINGS,
            skipped: true
        });
    }

    return {
        applySearchGroupSettings: applySearchGroupSettings,
        applySearchSortSettings: applySearchSortSettings,
        buildGroupSettingsFromEvent: buildGroupSettingsFromEvent,
        buildSortSettingsFromEvent: buildSortSettingsFromEvent
    };
});
