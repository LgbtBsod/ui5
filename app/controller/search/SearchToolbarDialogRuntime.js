sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/JsRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "sap/m/ViewSettingsDialog",
    "sap/m/ViewSettingsItem"
], function (JsRuntimeContracts, SearchToolbarContracts, ModelStateRuntime, OperationSourceContracts, ModelContracts, ViewSettingsDialog, ViewSettingsItem) {
    "use strict";

    var TYPE_FUNCTION = JsRuntimeContracts.TYPEOF.FUNCTION;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var TOKENS = ModelContracts.TOKENS;

    function resolveBundleText(oController, sTextKey, sFallback) {
        var oBundle = oController.getResourceBundle && oController.getResourceBundle();
        return (oBundle && oBundle.getText(sTextKey)) || sFallback;
    }

    function resolveItems(oController, aItems) {
        return (aItems || []).map(function (oItem) {
            return {
                key: oItem.key,
                text: resolveBundleText(oController, oItem.textKey, oItem.fallback)
            };
        });
    }

    function shouldRebindSearch(oController, ControllerViewStateRuntime) {
        return !!(ControllerViewStateRuntime.get(oController, "/hasSearched")
            && ControllerViewStateRuntime.get(oController, "/smartTableReady"));
    }

    function applySearchSortSettings(oController, mSettings, mDeps) {
        var sSortKey = String((mSettings && mSettings.sortKey) || "").trim() || TOKENS.DATE_CHECK;
        var bSortDescending = !!(mSettings && mSettings.sortDescending);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, sSortKey);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, bSortDescending);
        if (shouldRebindSearch(oController, mDeps.ControllerViewStateRuntime)) {
            mDeps.SearchCommandPolicy.rebind(oController, { source: OperationSourceContracts.SEARCH.SEARCH_SORT_SETTINGS });
        }
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
            mDeps.SearchCommandPolicy.rebind(oController, { source: OperationSourceContracts.SEARCH.SEARCH_GROUP_SETTINGS });
        }
    }

    function ensureDialog(oController, sDialogKey, mConfig) {
        var sInstanceKey = sDialogKey === "sort" ? "_oSearchSortDialog" : "_oSearchGroupDialog";
        if (!oController[sInstanceKey]) {
            oController[sInstanceKey] = new ViewSettingsDialog({
                title: resolveBundleText(oController, mConfig.titleKey, mConfig.titleFallback),
                confirm: mConfig.onConfirm.bind(oController)
            });
            resolveItems(oController, mConfig.items).forEach(function (oItem) {
                oController[sInstanceKey][mConfig.addItemMethod](new ViewSettingsItem({ key: oItem.key, text: oItem.text }));
            });
            oController.getView().addDependent(oController[sInstanceKey]);
        }
        return oController[sInstanceKey];
    }

    function openSortDialog(oController) {
        var oGroupDialog = oController._oSearchGroupDialog;
        var oDialog;
        if (oGroupDialog && typeof oGroupDialog.isOpen === TYPE_FUNCTION && oGroupDialog.isOpen()) {
            oGroupDialog.close();
        }
        oDialog = ensureDialog(oController, "sort", {
            titleKey: "searchSortDialogTitle",
            titleFallback: "Sort",
            items: SearchToolbarContracts.SORT_ITEMS,
            addItemMethod: "addSortItem",
            onConfirm: oController.onSearchSortDialogConfirm
        });
        oDialog.setSelectedSortItem(String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_KEY, SearchToolbarContracts.DEFAULTS.SORT_KEY) || SearchToolbarContracts.DEFAULTS.SORT_KEY));
        oDialog.setSortDescending(!!ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_SORT_DESCENDING, true));
        oDialog.open("sort");
    }

    function openGroupDialog(oController) {
        var oSortDialog = oController._oSearchSortDialog;
        var oDialog;
        if (oSortDialog && typeof oSortDialog.isOpen === TYPE_FUNCTION && oSortDialog.isOpen()) {
            oSortDialog.close();
        }
        oDialog = ensureDialog(oController, "group", {
            titleKey: "searchGroupDialogTitle",
            titleFallback: "Group",
            items: SearchToolbarContracts.GROUP_ITEMS,
            addItemMethod: "addGroupItem",
            onConfirm: oController.onSearchGroupDialogConfirm
        });
        oDialog.setSelectedGroupItem(String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_KEY, "") || SearchToolbarContracts.DEFAULTS.GROUP_KEY));
        oDialog.setGroupDescending(!!ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_GROUP_DESCENDING, false));
        oDialog.open("group");
    }

    return {
        applySearchSortSettings: applySearchSortSettings,
        applySearchGroupSettings: applySearchGroupSettings,
        openSortDialog: openSortDialog,
        openGroupDialog: openGroupDialog
    };
});
