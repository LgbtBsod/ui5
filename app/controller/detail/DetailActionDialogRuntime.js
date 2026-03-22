sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerReturnFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (ControllerReturnFocusRuntime, ControllerViewStateRuntime, FocusRuntime, SchedulingRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

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
                if (typeof oTreeTable.clearSelection === TYPE_FUNCTION) {
                    oTreeTable.clearSelection();
                }
                if (typeof oTreeTable.setFirstVisibleRow === TYPE_FUNCTION) {
                    oTreeTable.setFirstVisibleRow(0);
                }
                if (typeof oTreeTable.invalidate === TYPE_FUNCTION) {
                    oTreeTable.invalidate();
                }
            }.bind(this), 90);
        },

        _rememberDialogReturnFocus: function (sKey, oControl) {
            var oFallback = oControl;
            if (!sKey) {
                return;
            }
            if (!oFallback) {
                oFallback = FocusRuntime.createActiveElementFallback();
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
                oFocusTarget = oSearchField && typeof oSearchField[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION ? oSearchField[METHODS.GET_FOCUS_DOM_REF]() : null;
                if (oFocusTarget && typeof oFocusTarget[METHODS.FOCUS] === TYPE_FUNCTION) {
                    oFocusTarget[METHODS.FOCUS]();
                    return;
                }
                FocusRuntime.focusSoon(oSearchField);
            }, 60);
        }
    };
});
