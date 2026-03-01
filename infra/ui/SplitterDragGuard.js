sap.ui.define([], function () {
    "use strict";

    function debugLog(sEvent, oPayload) {
        if (window.__DEBUG_UI5__ === true && window.console && window.console.info) {
            window.console.info("[SplitterDragGuard] " + sEvent, oPayload || {});
        }
    }

    function hasSplitterBarTarget(oEvent) {
        var oTarget = oEvent && oEvent.target;
        var sClassName = (oTarget && oTarget.className) ? String(oTarget.className) : "";
        return sClassName.indexOf("sapUiLoSplitterBar") > -1;
    }

    function SplitterDragGuard(oSplitter) {
        this._oSplitter = oSplitter;
        this._fnStop = null;
        this._oDelegate = {
            onAfterRendering: this._wireDom.bind(this)
        };
    }

    SplitterDragGuard.prototype.start = function () {
        if (!this._oSplitter) {
            return;
        }
        this._oSplitter.addEventDelegate(this._oDelegate);
        debugLog("start");
    };

    SplitterDragGuard.prototype.stop = function () {
        if (this._oSplitter) {
            this._oSplitter.removeEventDelegate(this._oDelegate);
        }
        if (this._fnStop) {
            this._fnStop();
            this._fnStop = null;
        }
        debugLog("stop");
    };

    SplitterDragGuard.prototype._wireDom = function () {
        var oDomRef = this._oSplitter && this._oSplitter.getDomRef && this._oSplitter.getDomRef();
        if (!oDomRef) {
            return;
        }

        if (this._fnStop) {
            this._fnStop();
            this._fnStop = null;
        }

        var fnArm = function () {
            document.body.classList.add("appSplitterDragging");
            debugLog("arm");
        };
        var fnDisarm = function () {
            document.body.classList.remove("appSplitterDragging");
            debugLog("disarm");
        };

        var fnPointerDown = function (oEvent) {
            if (hasSplitterBarTarget(oEvent)) {
                fnArm();
            }
        };

        oDomRef.addEventListener("mousedown", fnPointerDown, true);
        window.addEventListener("mouseup", fnDisarm, true);
        window.addEventListener("mouseleave", fnDisarm, true);

        this._fnStop = function () {
            oDomRef.removeEventListener("mousedown", fnPointerDown, true);
            window.removeEventListener("mouseup", fnDisarm, true);
            window.removeEventListener("mouseleave", fnDisarm, true);
            fnDisarm();
        };
    };

    return SplitterDragGuard;
});
