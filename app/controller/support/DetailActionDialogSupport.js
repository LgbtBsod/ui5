sap.ui.define([
    "checklist/app/controller/support/ControllerReturnFocusRuntime",
    "checklist/app/controller/support/ControllerModelWriteSupport",
    "checklist/app/service/framework/FocusRuntime"
], function (ControllerReturnFocusRuntime, ControllerModelWriteSupport, FocusRuntime) {
    "use strict";

    return {
        _withViewFlag: function (sPath, fnWork) {
            return ControllerModelWriteSupport.withFlag(this, "view", sPath, fnWork, true, false);
        },

        _clearLocationValueHelpSearchTimer: function () {
            if (this._iLocationVhSearchTimer) {
                clearTimeout(this._iLocationVhSearchTimer);
                this._iLocationVhSearchTimer = null;
            }
        },

        _scheduleLocationValueHelpTableSync: function () {
            if (this._iLocationVhTableSyncTimer) {
                clearTimeout(this._iLocationVhTableSyncTimer);
            }
            this._iLocationVhTableSyncTimer = setTimeout(function () {
                var oTreeTable = this.byId("locationValueHelpTreeTable");
                this._iLocationVhTableSyncTimer = null;
                if (!oTreeTable) {
                    return;
                }
                if (typeof oTreeTable.clearSelection === "function") {
                    oTreeTable.clearSelection();
                }
                if (typeof oTreeTable.setFirstVisibleRow === "function") {
                    oTreeTable.setFirstVisibleRow(0);
                }
                if (typeof oTreeTable.invalidate === "function") {
                    oTreeTable.invalidate();
                }
            }.bind(this), 90);
        },

        _rememberDialogReturnFocus: function (sKey, oControl) {
            var oFallback = oControl;
            var oActive;
            var sActiveId;
            if (!sKey) {
                return;
            }
            if (!oFallback && typeof document !== "undefined") {
                oActive = document.activeElement;
                sActiveId = oActive && oActive.id ? String(oActive.id) : "";
                if (sActiveId) {
                    oFallback = {
                        focus: function () {
                            var oNode = document.getElementById(sActiveId);
                            if (oNode && typeof oNode.focus === "function") {
                                oNode.focus();
                            }
                        }
                    };
                } else if (oActive && typeof oActive.focus === "function") {
                    oFallback = oActive;
                }
            }
            ControllerReturnFocusRuntime.remember(this, sKey, oFallback, {
                storeProperty: "_mDialogReturnFocus"
            });
        },

        _restoreDialogFocus: function (sKey) {
            ControllerReturnFocusRuntime.restore(this, sKey, {
                storeProperty: "_mDialogReturnFocus"
            });
        },

        _onDialogAfterOpen: function (sKey) {
            var oSearchField;
            var oFocusTarget;
            if (sKey !== "locationValueHelp") {
                return;
            }
            oSearchField = this.byId("locationValueHelpSearchField");
            setTimeout(function () {
                oFocusTarget = oSearchField && oSearchField.getFocusDomRef ? oSearchField.getFocusDomRef() : null;
                if (oFocusTarget && typeof oFocusTarget.focus === "function") {
                    oFocusTarget.focus();
                    return;
                }
                FocusRuntime.focusSoon(oSearchField);
            }, 60);
        }
    };
});
