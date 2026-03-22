sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "sap/m/ViewSettingsDialog",
    "sap/m/ViewSettingsItem"
], function (SearchToolbarContracts, ModelStateRuntime, OperationSourceContracts, ModelContracts, JsRuntime, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, SearchMessageKeyConstants, ViewSettingsDialog, ViewSettingsItem) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var TOKENS = ModelContracts.TOKENS;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

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

    function openSortDialog(oController) {
        var oGroupDialog = oController._oSearchGroupDialog;
        var oDialog;
        if (oGroupDialog && typeof oGroupDialog.isOpen === TYPE_FUNCTION && oGroupDialog.isOpen()) {
            oGroupDialog.close();
        }
        oDialog = ensureDialog(oController, "sort", {
            titleKey: SearchMessageKeyConstants.SORT_DIALOG_TITLE,
            titleFallback: "",
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
            titleKey: SearchMessageKeyConstants.GROUP_DIALOG_TITLE,
            titleFallback: "",
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
        applySearchGroupSettings: applySearchGroupSettings,
        applySearchSortSettings: applySearchSortSettings,
        buildGroupSettingsFromEvent: buildGroupSettingsFromEvent,
        buildSortSettingsFromEvent: buildSortSettingsFromEvent,
        openGroupDialog: openGroupDialog,
        openSortDialog: openSortDialog
    };
});
