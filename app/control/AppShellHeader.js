sap.ui.define([
    "sap/ui/core/Control",
    "sap/m/OverflowToolbar",
    "sap/m/OverflowToolbarLayoutData",
    "sap/m/VBox",
    "sap/m/HBox",
    "sap/m/Text",
    "sap/m/Title",
    "sap/m/ObjectStatus",
    "sap/m/ToolbarSpacer",
    "sap/m/Button",
    "sap_ui5/control/ThemeToggle"
], function (
    Control,
    OverflowToolbar,
    OverflowToolbarLayoutData,
    VBox,
    HBox,
    Text,
    Title,
    ObjectStatus,
    ToolbarSpacer,
    Button,
    ThemeToggle
) {
    "use strict";

    function applyPriority(oControl, sPriority) {
        oControl.setLayoutData(new OverflowToolbarLayoutData({
            priority: sPriority
        }));
        return oControl;
    }

    return Control.extend("sap_ui5.control.AppShellHeader", {
        metadata: {
            properties: {
                eyebrow: { type: "string", defaultValue: "" },
                productName: { type: "string", defaultValue: "" },
                contextSubtitle: { type: "string", defaultValue: "" },
                routeLabel: { type: "string", defaultValue: "" },
                layoutLabel: { type: "string", defaultValue: "" },
                layoutState: { type: "string", defaultValue: "Information" },
                modeLabel: { type: "string", defaultValue: "" },
                modeState: { type: "string", defaultValue: "Information" },
                notificationsText: { type: "string", defaultValue: "" },
                helpText: { type: "string", defaultValue: "" },
                settingsText: { type: "string", defaultValue: "" },
                analyticsText: { type: "string", defaultValue: "" },
                userLabel: { type: "string", defaultValue: "" },
                userIcon: { type: "string", defaultValue: "sap-icon://employee" },
                notificationsTooltip: { type: "string", defaultValue: "" },
                helpTooltip: { type: "string", defaultValue: "" },
                settingsTooltip: { type: "string", defaultValue: "" },
                analyticsTooltip: { type: "string", defaultValue: "" },
                userTooltip: { type: "string", defaultValue: "" },
                dark: { type: "boolean", defaultValue: false },
                themeTooltip: { type: "string", defaultValue: "" }
            },
            aggregations: {
                _toolbar: { type: "sap.m.OverflowToolbar", multiple: false, visibility: "hidden" }
            },
            events: {
                notificationsPress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                },
                helpPress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                },
                settingsPress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                },
                analyticsPress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                },
                themePress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                },
                userPress: {
                    parameters: {
                        anchor: { type: "sap.ui.core.Control" }
                    }
                }
            }
        },

        init: function () {
            var that = this;

            this._oEyebrow = new Text().addStyleClass("shellEyebrow");
            this._oProductTitle = new Title({
                level: "H6"
            }).addStyleClass("shellProductTitle");
            this._oContextSubtitle = new Text().addStyleClass("shellContextSubtitle");

            this._oRouteStatus = applyPriority(new ObjectStatus({
                state: "Information"
            }).addStyleClass("shellContextChip"), "NeverOverflow");
            this._oLayoutStatus = applyPriority(new ObjectStatus().addStyleClass("shellContextChip"), "Low");
            this._oModeStatus = applyPriority(new ObjectStatus().addStyleClass("shellContextChip"), "Disappear");

            this._oNotificationsButton = applyPriority(new Button({
                icon: "sap-icon://bell",
                type: "Transparent",
                press: function () {
                    that.fireNotificationsPress({ anchor: that._oNotificationsButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("rnvRipple"), "High");

            this._oHelpButton = applyPriority(new Button({
                icon: "sap-icon://sys-help",
                type: "Transparent",
                press: function () {
                    that.fireHelpPress({ anchor: that._oHelpButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("rnvRipple"), "High");

            this._oSettingsButton = applyPriority(new Button({
                icon: "sap-icon://action-settings",
                type: "Transparent",
                press: function () {
                    that.fireSettingsPress({ anchor: that._oSettingsButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("rnvRipple"), "High");

            this._oAnalyticsButton = applyPriority(new Button({
                icon: "sap-icon://business-objects-experience",
                type: "Transparent",
                press: function () {
                    that.fireAnalyticsPress({ anchor: that._oAnalyticsButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("rnvRipple"), "High");

            this._oThemeToggle = new ThemeToggle();
            this._oThemeToggle.attachPress(function () {
                that.fireThemePress({ anchor: that._oThemeToggle });
            });

            this._oUserButton = applyPriority(new Button({
                type: "Transparent",
                press: function () {
                    that.fireUserPress({ anchor: that._oUserButton });
                }
            }).addStyleClass("shellUserBtn").addStyleClass("rnvRipple"), "NeverOverflow");

            this.setAggregation("_toolbar", new OverflowToolbar({
                content: [
                    new VBox({
                        renderType: "Bare",
                        items: [
                            this._oEyebrow,
                            this._oProductTitle,
                            this._oContextSubtitle
                        ]
                    }).addStyleClass("shellBrandCluster"),
                    new HBox({
                        renderType: "Bare",
                        wrap: "Wrap",
                        alignItems: "Center",
                        items: [
                            this._oRouteStatus,
                            this._oLayoutStatus,
                            this._oModeStatus
                        ]
                    }).addStyleClass("shellContextRail"),
                    new ToolbarSpacer(),
                    this._oNotificationsButton,
                    this._oHelpButton,
                    this._oSettingsButton,
                    this._oAnalyticsButton,
                    new VBox({
                        renderType: "Bare",
                        items: [this._oThemeToggle]
                    }).addStyleClass("shellThemeToggle"),
                    this._oUserButton
                ]
            }).addStyleClass("shellHeaderCard").addStyleClass("appShellHeader"));
        },

        onBeforeRendering: function () {
            this._oEyebrow.setText(this.getEyebrow());
            this._oProductTitle.setText(this.getProductName());
            this._oContextSubtitle.setText(this.getContextSubtitle());

            this._oRouteStatus.setText(this.getRouteLabel());
            this._oLayoutStatus.setText(this.getLayoutLabel());
            this._oLayoutStatus.setState(this.getLayoutState());
            this._oModeStatus.setText(this.getModeLabel());
            this._oModeStatus.setState(this.getModeState());

            this._oNotificationsButton.setTooltip(this.getNotificationsTooltip());
            this._oHelpButton.setTooltip(this.getHelpTooltip());
            this._oSettingsButton.setTooltip(this.getSettingsTooltip());
            this._oAnalyticsButton.setTooltip(this.getAnalyticsTooltip());
            this._oThemeToggle.setDark(this.getDark());
            this._oThemeToggle.setTooltip(this.getThemeTooltip());
            this._oUserButton.setText(this.getUserLabel());
            this._oUserButton.setTooltip(this.getUserTooltip());
        },

        setDark: function (bDark) {
            var bValue = !!bDark;
            this.setProperty("dark", bValue, true);
            if (this._oThemeToggle && this._oThemeToggle.setDark) {
                this._oThemeToggle.setDark(bValue);
            }
            return this;
        },

        setThemeTooltip: function (sTooltip) {
            var sValue = String(sTooltip || "");
            this.setProperty("themeTooltip", sValue, true);
            if (this._oThemeToggle && this._oThemeToggle.setTooltip) {
                this._oThemeToggle.setTooltip(sValue);
            }
            return this;
        },

        renderer: {
            apiVersion: 2,

            render: function (oRm, oControl) {
                oRm.renderControl(oControl.getAggregation("_toolbar"));
            }
        }
    });
});
