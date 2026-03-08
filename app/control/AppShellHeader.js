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
    "checklist/app/control/ThemeToggle"
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

    function syncHeaderContent(oControl) {
        if (!oControl) {
            return;
        }

        if (oControl._oEyebrow) {
            oControl._oEyebrow.setText(oControl.getEyebrow());
        }
        if (oControl._oProductTitle) {
            oControl._oProductTitle.setText(oControl.getProductName());
        }
        if (oControl._oContextSubtitle) {
            oControl._oContextSubtitle.setText(oControl.getContextSubtitle());
        }
        if (oControl._oRouteStatus) {
            oControl._oRouteStatus.setText(oControl.getRouteLabel());
        }
        if (oControl._oLayoutStatus) {
            oControl._oLayoutStatus.setText(oControl.getLayoutLabel());
            oControl._oLayoutStatus.setState(oControl.getLayoutState());
        }
        if (oControl._oModeStatus) {
            oControl._oModeStatus.setText(oControl.getModeLabel());
            oControl._oModeStatus.setState(oControl.getModeState());
        }
        if (oControl._oNotificationsButton) {
            oControl._oNotificationsButton.setTooltip(oControl.getNotificationsTooltip());
        }
        if (oControl._oHelpButton) {
            oControl._oHelpButton.setTooltip(oControl.getHelpTooltip());
        }
        if (oControl._oSettingsButton) {
            oControl._oSettingsButton.setTooltip(oControl.getSettingsTooltip());
        }
        if (oControl._oAnalyticsButton) {
            oControl._oAnalyticsButton.setTooltip(oControl.getAnalyticsTooltip());
        }
        if (oControl._oThemeToggle) {
            oControl._oThemeToggle.setDark(oControl.getDark());
            oControl._oThemeToggle.setTooltip(oControl.getThemeTooltip());
        }
        if (oControl._oUserButton) {
            oControl._oUserButton.setText(oControl.getUserLabel());
            oControl._oUserButton.setTooltip(oControl.getUserTooltip());
            oControl._oUserButton.setIcon(oControl.getUserIcon());
        }
    }

    return Control.extend("checklist.app.control.AppShellHeader", {
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
            }).addStyleClass("shellActionBtn").addStyleClass("chkRipple"), "High");

            this._oHelpButton = applyPriority(new Button({
                icon: "sap-icon://sys-help",
                type: "Transparent",
                press: function () {
                    that.fireHelpPress({ anchor: that._oHelpButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("chkRipple"), "High");

            this._oSettingsButton = applyPriority(new Button({
                icon: "sap-icon://action-settings",
                type: "Transparent",
                press: function () {
                    that.fireSettingsPress({ anchor: that._oSettingsButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("chkRipple"), "High");

            this._oAnalyticsButton = applyPriority(new Button({
                icon: "sap-icon://business-objects-experience",
                type: "Transparent",
                press: function () {
                    that.fireAnalyticsPress({ anchor: that._oAnalyticsButton });
                }
            }).addStyleClass("shellActionBtn").addStyleClass("chkRipple"), "High");

            this._oThemeToggle = new ThemeToggle();
            this._oThemeToggle.attachPress(function () {
                that.fireThemePress({ anchor: that._oThemeToggle });
            });

            this._oUserButton = applyPriority(new Button({
                type: "Transparent",
                press: function () {
                    that.fireUserPress({ anchor: that._oUserButton });
                }
            }).addStyleClass("shellUserBtn").addStyleClass("chkRipple"), "NeverOverflow");

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

            syncHeaderContent(this);
        },

        onBeforeRendering: function () {
            syncHeaderContent(this);
        },

        setProperty: function (sPropertyName, vValue, bSuppressInvalidate) {
            Control.prototype.setProperty.call(this, sPropertyName, vValue, true);
            syncHeaderContent(this);
            if (!bSuppressInvalidate) {
                this.invalidate();
            }
            return this;
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
