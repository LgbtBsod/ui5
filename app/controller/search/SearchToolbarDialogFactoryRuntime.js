sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ReadinessTelemetryConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "sap/m/ViewSettingsDialog",
    "sap/m/ViewSettingsItem"
], function (JsRuntime, SearchToolbarContracts, ModelStateRuntime, ModelContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, ViewSettingsDialog, ViewSettingsItem) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;

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
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DEFERRED_DIALOG_READY, {
            dialog: "searchSort"
        });
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
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DEFERRED_DIALOG_READY, {
            dialog: "searchGroup"
        });
    }

    return {
        openGroupDialog: openGroupDialog,
        openSortDialog: openSortDialog
    };
});
