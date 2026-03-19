sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime"
], function (ThemeDomRuntime) {
    "use strict";

    function resolveViewDom(oController) {
        return oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
    }

    function setSearchViewportCssVar(oController, sName, sValue) {
        var oViewDom = resolveViewDom(oController);
        ThemeDomRuntime.setStyleProperty([oViewDom], sName, sValue);
    }

    function resolveOuterHeight(oControl) {
        var oDomRef = oControl && oControl.getDomRef && oControl.getDomRef();
        if (!oDomRef || !oDomRef.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oDomRef.getBoundingClientRect().height || 0));
    }

    function resolveDomHeight(vControlOrDom) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        if (!oDomRef || !oDomRef.getBoundingClientRect) {
            return 0;
        }
        return Math.max(0, Math.ceil(oDomRef.getBoundingClientRect().height || 0));
    }

    function resolveSearchTableToolbarDom(oController) {
        var oToolbarHost = oController.byId && oController.byId("searchResultsToolbarHost");
        return oToolbarHost && oToolbarHost.getDomRef && oToolbarHost.getDomRef();
    }

    function resolveSearchSummaryRailDom(oController) {
        var oSummaryRail = oController.byId && oController.byId("searchResultsSummaryRail");
        return oSummaryRail && oSummaryRail.getDomRef && oSummaryRail.getDomRef();
    }

    function setSearchStickyTop(vControlOrDom, sTop) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        ThemeDomRuntime.setStyleProperty([oDomRef], "top", sTop);
        ThemeDomRuntime.setStyleProperty([oDomRef], "position", "sticky");
    }

    function setSearchStaticTop(vControlOrDom) {
        var oDomRef = vControlOrDom && vControlOrDom.nodeType ? vControlOrDom : (vControlOrDom && vControlOrDom.getDomRef && vControlOrDom.getDomRef());
        ThemeDomRuntime.setStyleProperties([oDomRef], {
            "position": "relative",
            "top": "auto"
        });
    }

    function resolveSearchWorkbenchDock(oController) {
        return oController.byId && oController.byId("searchWorkbenchDock");
    }

    function resolveShellHeaderHostDom(oController) {
        var oShellHeaderHost = oController && oController.byId && oController.byId("appShellHeaderHost");
        return oShellHeaderHost && oShellHeaderHost.getDomRef && oShellHeaderHost.getDomRef();
    }

    function resolveResultsTableToolbarHeight(oController) {
        return resolveDomHeight(resolveSearchTableToolbarDom(oController));
    }

    function resolveSearchViewportObserverTargets(oController, oScrollHost) {
        var aTargets = [];
        var oViewDom = resolveViewDom(oController);
        var oWorkbenchDock = resolveSearchWorkbenchDock(oController);
        var oWorkbenchDom = oWorkbenchDock && oWorkbenchDock.getDomRef && oWorkbenchDock.getDomRef();
        var oFilterCard = oController.byId && oController.byId("searchFilterCard");
        var oActionRail = oController.byId && oController.byId("searchResultsActionRail");
        var oResultsShell = oController.byId && oController.byId("searchResultsShell");
        var oShellHeader = resolveShellHeaderHostDom(oController);
        [oViewDom, oScrollHost, oWorkbenchDom, oShellHeader,
            oFilterCard && oFilterCard.getDomRef && oFilterCard.getDomRef(),
            oActionRail && oActionRail.getDomRef && oActionRail.getDomRef(),
            resolveSearchSummaryRailDom(oController),
            oResultsShell && oResultsShell.getDomRef && oResultsShell.getDomRef(),
            resolveSearchTableToolbarDom(oController)
        ].forEach(function (oTarget) {
            if (oTarget && aTargets.indexOf(oTarget) < 0) {
                aTargets.push(oTarget);
            }
        });
        return aTargets;
    }

    return {
        resolveDomHeight: resolveDomHeight,
        resolveOuterHeight: resolveOuterHeight,
        resolveResultsTableToolbarHeight: resolveResultsTableToolbarHeight,
        resolveSearchSummaryRailDom: resolveSearchSummaryRailDom,
        resolveSearchTableToolbarDom: resolveSearchTableToolbarDom,
        resolveSearchViewportObserverTargets: resolveSearchViewportObserverTargets,
        resolveShellHeaderHostDom: resolveShellHeaderHostDom,
        resolveViewDom: resolveViewDom,
        resolveSearchWorkbenchDock: resolveSearchWorkbenchDock,
        setSearchStaticTop: setSearchStaticTop,
        setSearchStickyTop: setSearchStickyTop,
        setSearchViewportCssVar: setSearchViewportCssVar
    };
});
