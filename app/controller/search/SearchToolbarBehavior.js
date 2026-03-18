sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (SearchToolbarDialogRuntime, ControllerViewStateRuntime, SearchCommandPolicy) {
    "use strict";

    function onSearchSortDialogConfirm(oController, oEvent) {
        var oSortItem = oEvent && oEvent.getParameter && oEvent.getParameter("sortItem");
        var bSortDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("sortDescending"));
        return SearchToolbarDialogRuntime.applySearchSortSettings(oController, {
            sortKey: oSortItem && oSortItem.getKey && oSortItem.getKey(),
            sortDescending: bSortDescending
        }, {
            ControllerViewStateRuntime: ControllerViewStateRuntime,
            SearchCommandPolicy: SearchCommandPolicy
        });
    }

    function onSearchGroupDialogConfirm(oController, oEvent) {
        var oGroupItem = oEvent && oEvent.getParameter && oEvent.getParameter("groupItem");
        var bGroupDescending = !!(oEvent && oEvent.getParameter && oEvent.getParameter("groupDescending"));
        return SearchToolbarDialogRuntime.applySearchGroupSettings(oController, {
            groupKey: oGroupItem && oGroupItem.getKey && oGroupItem.getKey(),
            groupDescending: bGroupDescending
        }, {
            ControllerViewStateRuntime: ControllerViewStateRuntime,
            SearchCommandPolicy: SearchCommandPolicy
        });
    }

    return {
        onSearchGroupDialogConfirm: onSearchGroupDialogConfirm,
        onSearchSortDialogConfirm: onSearchSortDialogConfirm
    };
});
