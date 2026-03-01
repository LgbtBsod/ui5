sap.ui.define([
    "sap/ui/core/Control",
    "sap/ui/core/IconPool"
], function (Control, IconPool) {
    "use strict";

    var oSunIcon = IconPool.getIconInfo("weather-sunny");
    var oMoonIcon = IconPool.getIconInfo("clear-night");
    var sSunContent = oSunIcon ? oSunIcon.content : "\ue0f4";
    var sMoonContent = oMoonIcon ? oMoonIcon.content : "\ue0f6";

    return Control.extend("sap_ui5.control.ThemeToggle", {
        metadata: {
            properties: {
                dark: { type: "boolean", defaultValue: false },
                tooltip: { type: "string", defaultValue: "" }
            },
            events: {
                press: {}
            }
        },

        onclick: function () {
            this.firePress();
        },

        onkeydown: function (oEvent) {
            var bIsEnter = oEvent.key === "Enter";
            var bIsSpace = oEvent.key === " " || oEvent.key === "Space" || oEvent.key === "Spacebar";

            if (!bIsEnter && !bIsSpace) {
                return;
            }

            oEvent.preventDefault();
            this.firePress();
        },

        renderer: {
            apiVersion: 2,

            render: function (oRm, oControl) {
                var bDark = oControl.getDark();
                var sTooltip = oControl.getTooltip();

                oRm.openStart("div", oControl);
                oRm.class("themeDockSwitch");
                if (bDark) {
                    oRm.class("isDark");
                }
                oRm.attr("role", "switch");
                oRm.attr("aria-checked", bDark ? "true" : "false");
                oRm.attr("tabindex", "0");
                if (sTooltip) {
                    oRm.attr("title", sTooltip);
                    oRm.attr("aria-label", sTooltip);
                }
                oRm.openEnd();

                oRm.openStart("div");
                oRm.class("themeDockSwitchTrack");
                oRm.openEnd();

                oRm.openStart("span");
                oRm.class("sapUiIcon");
                oRm.class("themeDockSwitchIcon");
                oRm.class("themeDockSwitchIconSun");
                oRm.attr("data-sap-ui-icon-content", sSunContent);
                oRm.attr("aria-hidden", "true");
                oRm.openEnd();
                oRm.close("span");

                oRm.openStart("span");
                oRm.class("sapUiIcon");
                oRm.class("themeDockSwitchIcon");
                oRm.class("themeDockSwitchIconMoon");
                oRm.attr("data-sap-ui-icon-content", sMoonContent);
                oRm.attr("aria-hidden", "true");
                oRm.openEnd();
                oRm.close("span");

                oRm.openStart("span");
                oRm.class("themeDockSwitchThumb");
                oRm.attr("aria-hidden", "true");
                oRm.openEnd();
                oRm.close("span");

                oRm.close("div");
                oRm.close("div");
            }
        }
    });
});
