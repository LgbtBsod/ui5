sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LazyDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerReturnFocusRuntime"
], function (FocusRuntime, LazyDialogRuntime, ControllerReturnFocusRuntime) {
    "use strict";

    return {
        _ensureShellOverlay: function (sKey, sFragmentName) {
            return LazyDialogRuntime.ensureDialog(this, sKey, {
                fragmentName: sFragmentName,
                cacheProperty: "_mShellOverlays",
                afterOpen: function (oOverlay, oController) {
                    oController._focusShellOverlay(sKey, oOverlay);
                },
                afterClose: function (_oOverlay, oController) {
                    oController._restoreShellOverlayFocus(sKey);
                }
            });
        },

        _openShellOverlay: function (oEvent, sKey, sFragmentName) {
            var oSource = (oEvent && oEvent.getParameter && oEvent.getParameter("anchor")) ||
                (oEvent && oEvent.getSource && oEvent.getSource());
            ControllerReturnFocusRuntime.remember(this, sKey, oSource, {
                storeProperty: "_mShellOverlayTriggers"
            });
            return this._ensureShellOverlay(sKey, sFragmentName).then(function (oOverlay) {
                if (oOverlay && typeof oOverlay.openBy === "function" && oSource) {
                    oOverlay.openBy(oSource);
                    return;
                }
                if (oOverlay && typeof oOverlay.open === "function") {
                    oOverlay.open();
                }
            });
        },

        _closeShellOverlay: function (sKey, bSkipFocusRestore) {
            var oOverlay = this._mShellOverlays && this._mShellOverlays[sKey];
            if (bSkipFocusRestore) {
                ControllerReturnFocusRuntime.markSkipRestore(this, sKey, {
                    skipProperty: "_mShellOverlaySkipRestore"
                });
            }
            if (oOverlay && oOverlay.close) {
                oOverlay.close();
            }
        },

        _focusShellOverlay: function (sKey, oOverlay) {
            var sTargetId = this._mShellOverlayFocusTargets && this._mShellOverlayFocusTargets[sKey];
            var oTarget = sTargetId && this.byId && this.byId(sTargetId);
            if (!oTarget && oOverlay && typeof oOverlay.focus === "function") {
                oTarget = oOverlay;
            }
            FocusRuntime.focusSoon(oTarget);
        },

        _restoreShellOverlayFocus: function (sKey) {
            ControllerReturnFocusRuntime.restore(this, sKey, {
                storeProperty: "_mShellOverlayTriggers",
                skipProperty: "_mShellOverlaySkipRestore"
            });
        }
    };
});
