sap.ui.define([], function () {
    "use strict";

    var _iResizeRafId = 0;
    var _iResizeEndTimer = 0;
    var _RESIZE_END_DELAY_MS = 120;
    var _RESIZE_CLASS = "rnvResizing";

    function _scheduleInvalidate(oLayout) {
        if (_iResizeRafId) {
            return;
        }
        _iResizeRafId = window.requestAnimationFrame(function () {
            _iResizeRafId = 0;
            if (oLayout && typeof oLayout.invalidate === "function") {
                oLayout.invalidate();
            }
        });
    }

    function _beginResizing() {
        var oDoc = document && document.documentElement;
        if (oDoc && oDoc.classList) {
            oDoc.classList.add(_RESIZE_CLASS);
        }
        if (_iResizeEndTimer) {
            window.clearTimeout(_iResizeEndTimer);
            _iResizeEndTimer = 0;
        }
    }

    function _scheduleResizeEnd() {
        if (_iResizeEndTimer) {
            window.clearTimeout(_iResizeEndTimer);
        }
        _iResizeEndTimer = window.setTimeout(function () {
            var oDoc = document && document.documentElement;

            _iResizeEndTimer = 0;
            if (oDoc && oDoc.classList) {
                oDoc.classList.remove(_RESIZE_CLASS);
            }
            if (window.Ui5Bg && typeof window.Ui5Bg.onResizeEnd === "function") {
                window.Ui5Bg.onResizeEnd();
            }
        }, _RESIZE_END_DELAY_MS);
    }

    return {
        _syncShellFlexAllocation: function () {
            var oLayout = this.byId("mainFcl");
            var oDomRef = oLayout && oLayout.getDomRef && oLayout.getDomRef();
            var oFlexItem = oDomRef && oDomRef.parentElement;
            if (!oFlexItem || !oFlexItem.classList || !oFlexItem.classList.contains("sapMFlexItem")) {
                return;
            }
            oFlexItem.style.flex = "1 1 auto";
            oFlexItem.style.minHeight = "0";
            oFlexItem.style.height = "auto";
        },

        _syncLayoutViewportGeometry: function () {
            var oLayout = this.byId("mainFcl");

            _beginResizing();
            if (window.Ui5Bg && typeof window.Ui5Bg.onResizeStart === "function") {
                window.Ui5Bg.onResizeStart();
            }
            _scheduleInvalidate(oLayout);
            _scheduleResizeEnd();
        },

        _syncStaticAreaScope: function () {
            var oCore = sap.ui.getCore && sap.ui.getCore();
            var oStaticArea = oCore && oCore.getStaticAreaRef && oCore.getStaticAreaRef();
            if (!oStaticArea || !oStaticArea.classList) {
                return;
            }
            oStaticArea.classList.add("rnvApp");
            oStaticArea.classList.add("rnvSkin");
        },

        _applyCompactDensityClass: function () {
            var oAppView = this._getAppViewModel();
            var bCompact = !!(oAppView && oAppView.getProperty && oAppView.getProperty("/compactDensity"));
            var oRoot = document && document.documentElement;
            var oBody = document && document.body;
            var oContainer = document && document.getElementById && document.getElementById("ui5_container");
            var oAppDom = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            [oRoot, oBody, oContainer, oAppDom].forEach(function (oNode) {
                if (oNode && oNode.classList) {
                    oNode.classList.toggle("appDensityCompact", bCompact);
                }
            });
        },

        _syncShellMetrics: function () {
            var oRoot = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            var oShellHeader = oRoot && oRoot.querySelector && oRoot.querySelector(".appShellHeader");
            var oDoc = document && document.documentElement;
            var oRect;
            var iBottom;
            if (!oDoc || !oShellHeader || !oShellHeader.getBoundingClientRect) {
                return;
            }
            oRect = oShellHeader.getBoundingClientRect();
            iBottom = Math.max(88, Math.round(oRect.bottom + 14));
            oDoc.style.setProperty("--app-shell-height", Math.round(oRect.height) + "px");
            oDoc.style.setProperty("--app-shell-offset", iBottom + "px");
        }
    };
});
