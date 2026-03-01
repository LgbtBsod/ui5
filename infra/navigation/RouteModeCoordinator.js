sap.ui.define([
    "sap_ui5/util/SplitterSizeService"
], function (SplitterSizeService) {
    "use strict";

    function debugLog(sEvent, oPayload) {
        if (window.__DEBUG_UI5__ === true && window.console && window.console.info) {
            window.console.info("[RouteModeCoordinator] " + sEvent, oPayload || {});
        }
    }

    function setResizable(oHost, bSplit) {
        var oLayoutData = oHost && oHost.getLayoutData && oHost.getLayoutData();
        if (oLayoutData && typeof oLayoutData.setResizable === "function") {
            oLayoutData.setResizable(bSplit);
        }
    }

    function applyVisualMode(oSplitter, oSearchPaneHost, oDetailPaneHost, sMode) {
        var bSplit = sMode === "split";
        if (oSplitter && typeof oSplitter.toggleStyleClass === "function") {
            oSplitter.toggleStyleClass("appSplitModeSingle", sMode === "single");
            oSplitter.toggleStyleClass("appSplitModeSplit", bSplit);
            oSplitter.toggleStyleClass("appSplitModeDetailOnly", sMode === "detailOnly");
        }
        setResizable(oSearchPaneHost, bSplit);
        setResizable(oDetailPaneHost, bSplit);
    }

    function RouteModeCoordinator(mDeps) {
        this._oRouter = mDeps.router;
        this._oStateModel = mDeps.stateModel;
        this._oSplitter = mDeps.splitter;
        this._oSearchPaneHost = mDeps.searchPaneHost;
        this._oDetailPaneHost = mDeps.detailPaneHost;

        this._fnRouteMatched = this._onAnyRouteMatched.bind(this);
        this._fnSplitterResize = this._onSplitterResized.bind(this);
    }

    RouteModeCoordinator.prototype.start = function () {
        if (!this._oStateModel) {
            return;
        }
        SplitterSizeService.restoreOnBoot(this._oStateModel);
        this._oRouter.attachRoutePatternMatched(this._fnRouteMatched);
        if (this._oSplitter && typeof this._oSplitter.attachResize === "function") {
            this._oSplitter.attachResize(this._fnSplitterResize);
        }
        applyVisualMode(this._oSplitter, this._oSearchPaneHost, this._oDetailPaneHost, this._oStateModel.getProperty("/splitLayoutMode") || "single");
        debugLog("start", { mode: this._oStateModel.getProperty("/splitLayoutMode") || "single" });
    };

    RouteModeCoordinator.prototype.stop = function () {
        if (this._oRouter) {
            this._oRouter.detachRoutePatternMatched(this._fnRouteMatched);
        }
        if (this._oSplitter && typeof this._oSplitter.detachResize === "function") {
            this._oSplitter.detachResize(this._fnSplitterResize);
        }
        debugLog("stop");
    };

    RouteModeCoordinator.prototype._onAnyRouteMatched = function (oEvent) {
        var sRouteName = oEvent.getParameter("name");
        var mArgs = oEvent.getParameter("arguments") || {};

        if (sRouteName === "search") {
            this._oStateModel.setProperty("/selectedId", null);
            SplitterSizeService.applyMode(this._oStateModel, "single");
        } else if (sRouteName === "detail" || sRouteName === "detailLayout") {
            this._oStateModel.setProperty("/selectedId", mArgs.id || null);
            SplitterSizeService.applyMode(this._oStateModel, "split");
        }

        applyVisualMode(this._oSplitter, this._oSearchPaneHost, this._oDetailPaneHost, this._oStateModel.getProperty("/splitLayoutMode") || "single");
        debugLog("routeMatched", {
            route: sRouteName,
            mode: this._oStateModel.getProperty("/splitLayoutMode"),
            selectedId: this._oStateModel.getProperty("/selectedId")
        });
    };

    RouteModeCoordinator.prototype._onSplitterResized = function () {
        if (this._oStateModel.getProperty("/splitLayoutMode") !== "split") {
            return;
        }

        var oLayoutData = this._oSearchPaneHost && this._oSearchPaneHost.getLayoutData && this._oSearchPaneHost.getLayoutData();
        var sLeft = oLayoutData && typeof oLayoutData.getSize === "function" ? oLayoutData.getSize() : "40%";
        var nLeft = parseInt(String(sLeft).replace("%", ""), 10);
        var mSizes = SplitterSizeService.syncFromSplitter(this._oStateModel, nLeft);
        SplitterSizeService.persistIfSplit(this._oStateModel);
        applyVisualMode(this._oSplitter, this._oSearchPaneHost, this._oDetailPaneHost, this._oStateModel.getProperty("/splitLayoutMode") || "single");

        debugLog("splitterResize", {
            left: mSizes.leftPct,
            right: mSizes.rightPct,
            mode: this._oStateModel.getProperty("/splitLayoutMode")
        });
    };

    return RouteModeCoordinator;
});
