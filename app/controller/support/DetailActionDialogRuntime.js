sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/ControllerReturnFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (ControllerReturnFocusRuntime, ControllerViewStateRuntime, FocusRuntime, SchedulingRuntime) {
    "use strict";

    return {
        _withViewFlag: function (sPath, fnWork) {
            return ControllerViewStateRuntime.withFlag(this, sPath, fnWork, true, false);
        },

        _clearLocationValueHelpSearchTimer: function () {
            this._iLocationVhSearchTimer = SchedulingRuntime.clearTimer(this._iLocationVhSearchTimer);
        },

        _scheduleLocationValueHelpTableSync: function () {
            this._iLocationVhTableSyncTimer = SchedulingRuntime.restartTimer(this._iLocationVhTableSyncTimer, function () {
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
            SchedulingRuntime.restartTimer(0, function () {
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
