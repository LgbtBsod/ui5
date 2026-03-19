sap.ui.define([
    "sap/ui/core/Control",
    "sap/m/OverflowToolbar",
    "sap/m/OverflowToolbarLayoutData",
    "sap/m/VBox",
    "sap/m/Text",
    "sap/m/Title",
    "sap/m/ToolbarSpacer",
    "sap/m/Button"
], function (
    Control,
    OverflowToolbar,
    OverflowToolbarLayoutData,
    VBox,
    Text,
    Title,
    ToolbarSpacer,
    Button
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
        if (oControl._oHelpButton) {
            oControl._oHelpButton.setTooltip(oControl.getHelpTooltip());
            oControl._oHelpButton.setVisible(!!oControl.getShowHelp());
        }
        if (oControl._oSettingsButton) {
            oControl._oSettingsButton.setTooltip(oControl.getSettingsTooltip());
        }
        if (oControl._oAnalyticsButton) {
            oControl._oAnalyticsButton.setTooltip(oControl.getAnalyticsTooltip());
        }
        if (oControl._oUserButton) {
            oControl._oUserButton.setText(oControl.getUserLabel());
            oControl._oUserButton.setTooltip(oControl.getUserTooltip());
            oControl._oUserButton.setIcon(oControl.getUserIcon());
        }
    }

    function setAndSync(oControl, sProperty, vValue) {
        oControl.setProperty(sProperty, vValue, true);
        syncHeaderContent(oControl);
        return oControl;
    }

    return Control.extend("PRODUCTION_CONTROL_CHECKLIST.controls.AppShellHeader", {
        metadata: {
            properties: {
                eyebrow: { type: "string", defaultValue: "" },
                productName: { type: "string", defaultValue: "" },
                contextSubtitle: { type: "string", defaultValue: "" },
                userLabel: { type: "string", defaultValue: "" },
                userIcon: { type: "string", defaultValue: "sap-icon://employee" },
                helpTooltip: { type: "string", defaultValue: "" },
                settingsTooltip: { type: "string", defaultValue: "" },
                analyticsTooltip: { type: "string", defaultValue: "" },
                userTooltip: { type: "string", defaultValue: "" },
                showHelp: { type: "boolean", defaultValue: false }
            },
            aggregations: {
                _toolbar: { type: "sap.m.OverflowToolbar", multiple: false, visibility: "hidden" }
            },
            events: {
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

            this._oHelpButton = applyPriority(new Button({
                icon: "sap-icon://sys-help",
                type: "Transparent",
                press: function () {
                    that.fireHelpPress({ anchor: that._oHelpButton });
                }
            }).addStyleClass("shellActionBtn"), "High");

            this._oSettingsButton = applyPriority(new Button({
                icon: "sap-icon://action-settings",
                type: "Transparent",
                press: function () {
                    that.fireSettingsPress({ anchor: that._oSettingsButton });
                }
            }).addStyleClass("shellActionBtn"), "High");

            this._oAnalyticsButton = applyPriority(new Button({
                icon: "sap-icon://business-objects-experience",
                type: "Transparent",
                press: function () {
                    that.fireAnalyticsPress({ anchor: that._oAnalyticsButton });
                }
            }).addStyleClass("shellActionBtn shellAnalyticsBtn"), "High");

            this._oUserButton = applyPriority(new Button({
                type: "Transparent",
                press: function () {
                    that.fireUserPress({ anchor: that._oUserButton });
                }
            }).addStyleClass("shellUserBtn"), "NeverOverflow");

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
                    new ToolbarSpacer().addStyleClass("shellHeaderSpacer"),
                    this._oHelpButton,
                    this._oSettingsButton,
                    this._oAnalyticsButton,
                    this._oUserButton
                ]
            }).addStyleClass("appShellHeader"));

            syncHeaderContent(this);
        },

        setEyebrow: function (sValue) {
            return setAndSync(this, "eyebrow", sValue);
        },

        setProductName: function (sValue) {
            return setAndSync(this, "productName", sValue);
        },

        setContextSubtitle: function (sValue) {
            return setAndSync(this, "contextSubtitle", sValue);
        },

        setUserLabel: function (sValue) {
            return setAndSync(this, "userLabel", sValue);
        },

        setUserIcon: function (sValue) {
            return setAndSync(this, "userIcon", sValue);
        },

        setHelpTooltip: function (sValue) {
            return setAndSync(this, "helpTooltip", sValue);
        },

        setSettingsTooltip: function (sValue) {
            return setAndSync(this, "settingsTooltip", sValue);
        },

        setAnalyticsTooltip: function (sValue) {
            return setAndSync(this, "analyticsTooltip", sValue);
        },

        setUserTooltip: function (sValue) {
            return setAndSync(this, "userTooltip", sValue);
        },

        setShowHelp: function (bValue) {
            return setAndSync(this, "showHelp", !!bValue);
        },

        renderer: {
            apiVersion: 2,

            render: function (oRm, oControl) {
                oRm.openStart("header", oControl);
                oRm.class("appShellHeaderRegion");
                oRm.attr("role", "banner");
                oRm.openEnd();
                oRm.renderControl(oControl.getAggregation("_toolbar"));
                oRm.close("header");
            }
        }
    });
});
