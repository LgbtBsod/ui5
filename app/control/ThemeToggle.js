sap.ui.define([
    "sap/ui/core/Control"
], function (Control) {
    "use strict";

    var sSunContent = "";
    var sMoonContent = "";

    return Control.extend("PRODUCTION_CONTROL_CHECKLIST.control.ThemeToggle", {
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

        setDark: function (bDark) {
            var bValue = !!bDark;
            var oDomRef = this.getDomRef && this.getDomRef();

            this.setProperty("dark", bValue, true);
            if (oDomRef) {
                oDomRef.classList.toggle("isDark", bValue);
                oDomRef.setAttribute("aria-checked", bValue ? "true" : "false");
            }
            return this;
        },

        setTooltip: function (sTooltip) {
            var sValue = String(sTooltip || "");
            var oDomRef = this.getDomRef && this.getDomRef();

            this.setProperty("tooltip", sValue, true);
            if (oDomRef) {
                if (sValue) {
                    oDomRef.setAttribute("title", sValue);
                    oDomRef.setAttribute("aria-label", sValue);
                } else {
                    oDomRef.removeAttribute("title");
                    oDomRef.removeAttribute("aria-label");
                }
            }
            return this;
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
                oRm.class("themeDockSwitchIcon");
                oRm.class("themeDockSwitchIconSun");
                oRm.attr("aria-hidden", "true");
                oRm.openEnd();
                oRm.text(sSunContent);
                oRm.close("span");

                oRm.openStart("span");
                oRm.class("themeDockSwitchIcon");
                oRm.class("themeDockSwitchIconMoon");
                oRm.attr("aria-hidden", "true");
                oRm.openEnd();
                oRm.text(sMoonContent);
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
