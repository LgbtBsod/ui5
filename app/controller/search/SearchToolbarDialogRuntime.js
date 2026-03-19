sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarDialogFactoryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchToolbarSettingsRuntime"
], function (SearchToolbarDialogFactoryRuntime, SearchToolbarSettingsRuntime) {
    "use strict";

    return {
        applySearchGroupSettings: SearchToolbarSettingsRuntime.applySearchGroupSettings,
        applySearchSortSettings: SearchToolbarSettingsRuntime.applySearchSortSettings,
        buildGroupSettingsFromEvent: SearchToolbarSettingsRuntime.buildGroupSettingsFromEvent,
        buildSortSettingsFromEvent: SearchToolbarSettingsRuntime.buildSortSettingsFromEvent,
        openGroupDialog: SearchToolbarDialogFactoryRuntime.openGroupDialog,
        openSortDialog: SearchToolbarDialogFactoryRuntime.openSortDialog
    };
});
