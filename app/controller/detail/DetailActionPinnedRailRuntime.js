sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime"
], function (ThemeDomRuntime, EventDelegateRuntime) {
    "use strict";

    function resolveStickyHost(oController) {
        return (oController.byId && (oController.byId("detailControlPinnedDock") || oController.byId("detailControlStickyHost"))) || null;
    }

    function clearPinnedClasses(oController) {
        var oStickyHost = resolveStickyHost(oController);
        var oHostDom = oStickyHost && oStickyHost.getDomRef && oStickyHost.getDomRef();
        if (!oHostDom) {
            return;
        }
        ThemeDomRuntime.removeClass([oHostDom], "isViewportPinned");
        oHostDom.style.removeProperty("height");
        oHostDom.style.removeProperty("--detail-rail-height");
        ThemeDomRuntime.removeClass([oHostDom], "detailControlCardDocked");
    }

    return {
        _clearViewportPinnedControlRailRetry: function () {},

        _scheduleViewportPinnedControlRailBind: function () {
            clearPinnedClasses(this);
        },

        _bindDetailEditSwitchKeyboardFallback: function () {
            var oSwitch = this.byId("detailEditSwitch");
            if (!oSwitch || !oSwitch.addEventDelegate) {
                return;
            }
            if (!this._oDetailEditSwitchDelegate) {
                this._oDetailEditSwitchDelegate = {
                    onsapenter: this._onDetailEditSwitchKeyboardActivate.bind(this),
                    onsapspace: this._onDetailEditSwitchKeyboardActivate.bind(this)
                };
            }
            EventDelegateRuntime.ensure(this, "_oDetailEditSwitchDelegate", oSwitch, this._oDetailEditSwitchDelegate, this);
        },

        _unbindViewportPinnedControlRail: function () {
            var oSwitch = this.byId && this.byId("detailEditSwitch");
            EventDelegateRuntime.remove(this, "_oDetailEditSwitchDelegate", oSwitch);
            clearPinnedClasses(this);
        },

        _bindViewportPinnedControlRail: function () {
            clearPinnedClasses(this);
        },

        _syncViewportPinnedControlRail: function () {
            clearPinnedClasses(this);
        },

        _onDetailEditSwitchKeyboardActivate: function (oEvent) {
            var oSwitch = this.byId("detailEditSwitch");
            if (!oSwitch || !oSwitch.getEnabled || !oSwitch.getEnabled()) {
                return;
            }
            if (oEvent && oEvent.preventDefault) {
                oEvent.preventDefault();
            }
            if (oEvent && oEvent.stopPropagation) {
                oEvent.stopPropagation();
            }
            oSwitch.fireChange({ state: !oSwitch.getState() });
        }
    };
});
