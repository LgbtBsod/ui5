sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ShellPaneContracts",
    "sap/ui/dom/includeStylesheet"
], function (ShellPaneContracts, includeStylesheet) {
    "use strict";

    var mLoadedPanes = Object.create(null);

    function resolveStyleHref(sModulePath) {
        return sap.ui.require.toUrl(sModulePath);
    }

    function ensurePaneStyles(sPaneKey) {
        var sModulePath = ShellPaneContracts.STYLE_MODULES[sPaneKey];
        var sStyleId = ShellPaneContracts.STYLE_IDS[sPaneKey];
        if (!sModulePath || !sStyleId || mLoadedPanes[sPaneKey]) {
            return false;
        }
        includeStylesheet(resolveStyleHref(sModulePath), sStyleId);
        mLoadedPanes[sPaneKey] = true;
        return true;
    }

    return {
        ensurePaneStyles: ensurePaneStyles
    };
});
