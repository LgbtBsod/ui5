sap.ui.define([], function () {
    "use strict";

    function _logInfo(sMessage, oData) {
        if (window.jQuery && jQuery.sap && jQuery.sap.log && jQuery.sap.log.info) {
            jQuery.sap.log.info(sMessage, oData ? JSON.stringify(oData) : "", "sap_ui5.util.FclResizer");
            return;
        }
        if (window.console && window.console.info) {
            window.console.info("[FclResizer] " + sMessage, oData || {});
        }
    }

    function FclResizer(oFcl, mOptions) {
        this._oFcl = oFcl;
        this._mOptions = Object.assign({
            appId: "sap_ui5",
            minWidthPct: 15,
            maxWidthPct: 70
        }, mOptions || {});

        this._fnAfterRendering = this._initHandles.bind(this);
        this._oRenderDelegate = { onAfterRendering: this._fnAfterRendering };
        this._mDrag = null;
        this._aHandles = [];
        this._sStoragePrefix = "FCL_SIZES_" + this._mOptions.appId + "_";
    }

    FclResizer.prototype.attach = function () {
        if (!this._oFcl || this._bAttached) {
            return;
        }
        this._oFcl.addEventDelegate(this._oRenderDelegate, this);
        this._bAttached = true;
        this._initHandles();
    };

    FclResizer.prototype.detach = function () {
        if (this._oFcl && this._oRenderDelegate && this._bAttached) {
            this._oFcl.removeEventDelegate(this._oRenderDelegate, this);
        }
        this._bAttached = false;
        this._destroyHandles();
        this._removeDragListeners();
    };

    FclResizer.prototype._initHandles = function () {
        var oDom = this._oFcl && this._oFcl.getDomRef && this._oFcl.getDomRef();
        if (!oDom) {
            return;
        }

        this._destroyHandles();

        var oBegin = oDom.querySelector(".sapFFCLColumnBegin");
        var oMid = oDom.querySelector(".sapFFCLColumnMid");
        var oEnd = oDom.querySelector(".sapFFCLColumnEnd");
        if (!oBegin || !oMid) {
            return;
        }

        this._createHandle(oDom, "begin-mid", function (oEvent) {
            this._startDrag(oEvent, "begin-mid", oDom, oBegin, oMid, oEnd);
        }.bind(this));

        if (oEnd) {
            this._createHandle(oDom, "mid-end", function (oEvent) {
                this._startDrag(oEvent, "mid-end", oDom, oBegin, oMid, oEnd);
            }.bind(this));
        }

        this._applySavedSizes(oDom, oBegin, oMid, oEnd);
        this._positionHandles();
        _logInfo("FCL resize handles initialized");
    };

    FclResizer.prototype._createHandle = function (oContainer, sType, fnDown) {
        var oHandle = document.createElement("div");
        oHandle.className = "fclResizeHandle fclResizeHandle--" + sType;
        oHandle.setAttribute("data-handle-type", sType);
        oHandle.addEventListener("pointerdown", fnDown);
        oHandle.addEventListener("dblclick", function () {
            this._resetToDefault();
        }.bind(this));
        oContainer.appendChild(oHandle);
        this._aHandles.push({ type: sType, dom: oHandle, down: fnDown });
    };

    FclResizer.prototype._destroyHandles = function () {
        this._aHandles.forEach(function (oItem) {
            if (oItem.dom && oItem.down) {
                oItem.dom.removeEventListener("pointerdown", oItem.down);
            }
            if (oItem.dom && oItem.dom.parentNode) {
                oItem.dom.parentNode.removeChild(oItem.dom);
            }
        });
        this._aHandles = [];
    };

    FclResizer.prototype._startDrag = function (oEvent, sType, oContainer, oBegin, oMid, oEnd) {
        if (oEvent.button !== 0) {
            return;
        }
        oEvent.preventDefault();

        this._mDrag = {
            type: sType,
            container: oContainer,
            begin: oBegin,
            mid: oMid,
            end: oEnd
        };

        this._fnMove = this._onDragMove.bind(this);
        this._fnUp = this._onDragUp.bind(this);

        window.addEventListener("pointermove", this._fnMove);
        window.addEventListener("pointerup", this._fnUp);
    };

    FclResizer.prototype._onDragMove = function (oEvent) {
        if (!this._mDrag) {
            return;
        }

        var oRect = this._mDrag.container.getBoundingClientRect();
        var iMin = Number(this._mOptions.minWidthPct) || 15;
        var iMax = Number(this._mOptions.maxWidthPct) || 70;

        if (this._mDrag.type === "begin-mid") {
            var iBeginPct = Math.round(((oEvent.clientX - oRect.left) / oRect.width) * 100);
            iBeginPct = Math.max(iMin, Math.min(iMax, iBeginPct));
            var iRemaining = 100 - iBeginPct;
            if (iRemaining < iMin) {
                iRemaining = iMin;
                iBeginPct = 100 - iRemaining;
            }
            this._setColumnWidth(this._mDrag.begin, iBeginPct);
            this._setColumnWidth(this._mDrag.mid, iRemaining);
        }

        if (this._mDrag.type === "mid-end" && this._mDrag.end) {
            var iMidPct = Math.round(((oEvent.clientX - oRect.left) / oRect.width) * 100);
            iMidPct = Math.max(iMin, Math.min(iMax, iMidPct));
            var iEndPct = 100 - iMidPct;
            if (iEndPct < iMin) {
                iEndPct = iMin;
                iMidPct = 100 - iEndPct;
            }
            this._setColumnWidth(this._mDrag.mid, iMidPct);
            this._setColumnWidth(this._mDrag.end, iEndPct);
        }

        this._positionHandles();
    };

    FclResizer.prototype._onDragUp = function () {
        if (!this._mDrag) {
            return;
        }
        this._saveSizes(this._mDrag.container, this._mDrag.begin, this._mDrag.mid, this._mDrag.end);
        this._mDrag = null;
        this._removeDragListeners();
    };

    FclResizer.prototype._removeDragListeners = function () {
        if (this._fnMove) {
            window.removeEventListener("pointermove", this._fnMove);
            this._fnMove = null;
        }
        if (this._fnUp) {
            window.removeEventListener("pointerup", this._fnUp);
            this._fnUp = null;
        }
    };

    FclResizer.prototype._setColumnWidth = function (oColumnDom, iPct) {
        if (!oColumnDom || this._isHidden(oColumnDom)) {
            return;
        }
        oColumnDom.style.flex = "0 0 " + iPct + "%";
        oColumnDom.style.maxWidth = iPct + "%";
        oColumnDom.style.width = iPct + "%";
    };

    FclResizer.prototype._isHidden = function (oDom) {
        if (!oDom) {
            return true;
        }
        return oDom.offsetParent === null || window.getComputedStyle(oDom).display === "none";
    };

    FclResizer.prototype._positionHandles = function () {
        var oContainer = this._oFcl && this._oFcl.getDomRef && this._oFcl.getDomRef();
        if (!oContainer) {
            return;
        }
        var oRect = oContainer.getBoundingClientRect();
        if (!oRect.width) {
            return;
        }

        var oBegin = oContainer.querySelector(".sapFFCLColumnBegin");
        var oMid = oContainer.querySelector(".sapFFCLColumnMid");
        var oEnd = oContainer.querySelector(".sapFFCLColumnEnd");

        this._aHandles.forEach(function (oItem) {
            var oHandle = oItem.dom;
            if (!oHandle) {
                return;
            }
            if (oItem.type === "begin-mid") {
                if (this._isHidden(oBegin) || this._isHidden(oMid)) {
                    oHandle.style.display = "none";
                    return;
                }
                oHandle.style.display = "block";
                oHandle.style.left = Math.round(oBegin.getBoundingClientRect().right - oRect.left - 3) + "px";
            }
            if (oItem.type === "mid-end") {
                if (this._isHidden(oMid) || this._isHidden(oEnd)) {
                    oHandle.style.display = "none";
                    return;
                }
                oHandle.style.display = "block";
                oHandle.style.left = Math.round(oMid.getBoundingClientRect().right - oRect.left - 3) + "px";
            }
        }.bind(this));
    };

    FclResizer.prototype._resetToDefault = function () {
        var oContainer = this._oFcl && this._oFcl.getDomRef && this._oFcl.getDomRef();
        if (!oContainer) {
            return;
        }
        var oBegin = oContainer.querySelector(".sapFFCLColumnBegin");
        var oMid = oContainer.querySelector(".sapFFCLColumnMid");
        var oEnd = oContainer.querySelector(".sapFFCLColumnEnd");

        [oBegin, oMid, oEnd].forEach(function (oCol) {
            if (!oCol) {
                return;
            }
            oCol.style.flex = "";
            oCol.style.maxWidth = "";
            oCol.style.width = "";
        });

        var sKey = this._storageKey();
        try {
            window.localStorage.removeItem(sKey);
        } catch (e) {
            // ignore
        }
        this._positionHandles();
    };

    FclResizer.prototype._storageKey = function () {
        var sLayout = (this._oFcl && this._oFcl.getLayout && this._oFcl.getLayout()) || "OneColumn";
        return this._sStoragePrefix + sLayout;
    };

    FclResizer.prototype._saveSizes = function (oContainer, oBegin, oMid, oEnd) {
        var oRect = oContainer.getBoundingClientRect();
        if (!oRect.width) {
            return;
        }
        var m = {};
        if (oBegin && !this._isHidden(oBegin)) {
            m.begin = Math.round((oBegin.getBoundingClientRect().width / oRect.width) * 100);
        }
        if (oMid && !this._isHidden(oMid)) {
            m.mid = Math.round((oMid.getBoundingClientRect().width / oRect.width) * 100);
        }
        if (oEnd && !this._isHidden(oEnd)) {
            m.end = Math.round((oEnd.getBoundingClientRect().width / oRect.width) * 100);
        }
        try {
            window.localStorage.setItem(this._storageKey(), JSON.stringify(m));
        } catch (e) {
            // ignore
        }
    };

    FclResizer.prototype._applySavedSizes = function (oContainer, oBegin, oMid, oEnd) {
        var m = null;
        try {
            m = JSON.parse(window.localStorage.getItem(this._storageKey()) || "null");
        } catch (e) {
            m = null;
        }
        if (!m || typeof m !== "object") {
            return;
        }
        if (Number.isFinite(m.begin)) {
            this._setColumnWidth(oBegin, m.begin);
        }
        if (Number.isFinite(m.mid)) {
            this._setColumnWidth(oMid, m.mid);
        }
        if (Number.isFinite(m.end)) {
            this._setColumnWidth(oEnd, m.end);
        }
    };

    return FclResizer;
});
