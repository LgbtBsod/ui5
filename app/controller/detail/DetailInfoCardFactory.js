sap.ui.define([
    "sap/f/GridListItem",
    "sap/m/VBox",
    "sap/m/HBox",
    "sap/m/Text",
    "sap/m/Button",
    "sap/ui/core/CustomData",
    "sap/m/ObjectStatus",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReadSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DetailRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailObserverCardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailSimpleCardRuntime"
], function (GridListItem, VBox, HBox, Text, Button, CustomData, ObjectStatus, BindingContextReadSupport, ControlStyleRuntime, ControllerTextRuntime, DetailRuntimeContracts, ModelContracts, WorkflowContracts, DetailObserverCardRuntime, DetailSimpleCardRuntime) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var CARD_REQUIRED_KEYS = DetailRuntimeContracts.CARD_REQUIRED_KEYS;

    function withStyleClasses(oControl, sClassNames) {
        return ControlStyleRuntime.enable(oControl, sClassNames);
    }

    function normalizeValidationPath(sPath) {
        return "/" + String(sPath || "").replace(/^\//, "");
    }

    function toValidationKey(sPath) {
        return normalizeValidationPath(sPath).replace(/^\//, "").replace(/\//g, ".");
    }

    function resolveValidationKey(sValidationKey) {
        return toValidationKey(sValidationKey);
    }

    function isRequiredValidationKey(aRequiredFields, sValidationKey) {
        var sExpectedKey = resolveValidationKey(sValidationKey);
        var aRequired = Array.isArray(aRequiredFields) ? aRequiredFields : [];
        if (!sExpectedKey) {
            return false;
        }
        return aRequired.some(function (sPath) {
            return toValidationKey(sPath) === sExpectedKey;
        });
    }

    function bindValidation(oController, oControl, sValidationKey) {
        var sKey = resolveValidationKey(sValidationKey);
        oControl.bindProperty("valueState", {
            parts: [
                { path: "view>/validationShown" },
                { path: "view>/validationMissing/" + sKey }
            ],
            formatter: oController.formatValidationState.bind(oController)
        });
        oControl.bindProperty("valueStateText", {
            parts: [
                { path: "view>/validationShown" },
                { path: "view>/validationMissing/" + sKey }
            ],
            formatter: oController.formatValidationText.bind(oController)
        });
        return oControl;
    }

    function bindValidationMessage(oController, oText, sValidationKey) {
        var sKey = resolveValidationKey(sValidationKey);
        oText.bindProperty("visible", {
            parts: [
                { path: "view>/validationShown" },
                { path: "view>/validationMissing/" + sKey }
            ],
            formatter: function (bShown, bMissing) {
                return !!(bShown && bMissing);
            }
        });
        oText.bindProperty("text", {
            parts: [
                { path: "view>/validationShown" },
                { path: "view>/validationMissing/" + sKey }
            ],
            formatter: oController.formatValidationText.bind(oController)
        });
        return oText;
    }

    function bindRequiredState(oControl, sValidationKey) {
        var oMetadata;
        if (!oControl || !oControl.getMetadata || !oControl.bindProperty) {
            return oControl;
        }
        oMetadata = oControl.getMetadata();
        if (!oMetadata || !oMetadata.hasProperty || !oMetadata.hasProperty("required")) {
            return oControl;
        }
        oControl.bindProperty("required", {
            path: "state>/requiredFields",
            formatter: function (aRequiredFields) {
                return isRequiredValidationKey(aRequiredFields, sValidationKey);
            }
        });
        return oControl;
    }

    function wrapEditableField(oController, oControl, sValidationKey, sExtraClass) {
        var oShell = withStyleClasses(new VBox({ renderType: "Bare" }), ["detailFieldStack", sExtraClass].filter(Boolean).join(" "));
        var oMessage = withStyleClasses(new Text(), "detailFieldValidationText");
        var sKey = resolveValidationKey(sValidationKey);
        if (oControl && oControl.data) {
            oControl.data("validationKey", sKey || "");
        }
        bindRequiredState(oControl, sKey);
        bindValidation(oController, oControl, sKey);
        bindValidationMessage(oController, oMessage, sKey);
        oShell.addItem(oControl);
        oShell.addItem(oMessage);
        return oShell;
    }

    function createPrefixedStatus(oController, sLabelKey, sValuePath) {
        var sLabel = ControllerTextRuntime.getText(oController, sLabelKey, [], sLabelKey);
        var oStatus = new ObjectStatus({ state: "Information" });
        oStatus.bindProperty("text", {
            path: sValuePath,
            formatter: function (sValue) {
                return sLabel + ": " + (sValue || "-");
            }
        });
        return oStatus;
    }

    function isCardRequired(aRequiredFields, sCardKey) {
        var aPaths = CARD_REQUIRED_KEYS[sCardKey] || [];
        var aRequired = Array.isArray(aRequiredFields) ? aRequiredFields : [];
        if (!aPaths.length || !aRequired.length) {
            return false;
        }
        return aPaths.some(function (sPath) {
            return aRequired.indexOf(sPath) >= 0 || aRequired.indexOf(normalizeValidationPath(sPath)) >= 0;
        });
    }

    function formatRequiredCardTitle(sTitle, aRequiredFields, sCardKey) {
        var sBaseTitle = String(sTitle || "");
        if (!sBaseTitle) {
            return sBaseTitle;
        }
        return isCardRequired(aRequiredFields, sCardKey) ? (sBaseTitle + " *") : sBaseTitle;
    }

    function createInfoCardWrapper(sId, oContext) {
        var oItem = new GridListItem(sId, {
            type: "Active",
            customData: [
                new CustomData({
                    key: "cardkey",
                    value: "{" + VIEW_MODEL + ">key}",
                    writeToDom: true
                })
            ]
        });
        withStyleClasses(oItem, "infoCardGridItem");
        oItem.setBindingContext(oContext, VIEW_MODEL);
        oItem.addEventDelegate({
            onAfterRendering: function () {
                var oDomRef = oItem.getDomRef();
                if (oDomRef) {
                    oDomRef.setAttribute("tabindex", "0");
                }
            }
        });
        return oItem;
    }

    return {
        buildInfoCard: function (oController, sId, oContext) {
            var sKey = String(BindingContextReadSupport.read(oContext, "key", "") || "");
            var oItem = createInfoCardWrapper(sId, oContext);
            var oShell = withStyleClasses(new VBox({ renderType: "Bare" }), "infoCardTile mushroomCard");
            var oTitle = withStyleClasses(new Text(), "infoCardTitle");
            var oHeaderRow = withStyleClasses(new HBox({
                justifyContent: "SpaceBetween",
                alignItems: "Center",
                renderType: "Bare"
            }), "infoCardHeaderRow");
            var oPinButton = withStyleClasses(new Button({
                type: "Transparent",
                press: [oController.onToggleInfoCardPin, oController]
            }), "infoCardPinBtn");
            var aContent;

            oTitle.bindProperty("text", {
                parts: [
                    { path: "view>title" },
                    { path: "state>/requiredFields" }
                ],
                formatter: function (sTitle, aRequiredFields) {
                    return formatRequiredCardTitle(sTitle, aRequiredFields, sKey);
                }
            });
            oPinButton.bindProperty("visible", {
                path: "state>/workflow/detail/editMode",
                formatter: function (sMode) {
                    return sMode !== WorkflowContracts.EDIT_MODES.READ;
                }
            });
            oPinButton.bindProperty("icon", {
                path: "view>pinned",
                formatter: function (bPinned) {
                    return bPinned ? "sap-icon://pushpin-on" : "sap-icon://pushpin-off";
                }
            });
            oPinButton.bindProperty("tooltip", {
                path: "view>pinned",
                formatter: function (bPinned) {
                    return ControllerTextRuntime.getText(
                        oController,
                        bPinned ? "infoCardUnpinTooltip" : "infoCardPinTooltip",
                        [],
                        bPinned ? "Unpin card" : "Pin card"
                    );
                }
            });

            oHeaderRow.addItem(oTitle);
            oHeaderRow.addItem(oPinButton);
            oShell.addItem(oHeaderRow);
            oItem.attachBrowserEvent("keydown", function (oEvent) {
                var oTarget = oEvent && oEvent.target;
                if (oTarget && oTarget.closest && oTarget.closest(".sapMInputBase,.sapMSlt,.sapMBtn,.sapMSwt")) {
                    return;
                }
                oController.onInfoCardKeyDown(oEvent, sKey);
            });
            oItem.attachBrowserEvent("click", function (oEvent) {
                oController.onInfoCardPress(oEvent, sKey, oItem);
            });

            if (sKey === "observer" || sKey === "observed") {
                oShell.addItem(DetailObserverCardRuntime.createContent(oController, sKey, {
                    wrapEditableField: wrapEditableField
                }));
            } else {
                aContent = DetailSimpleCardRuntime.createContent(oController, sKey, {
                    wrapEditableField: wrapEditableField,
                    createPrefixedStatus: createPrefixedStatus
                });
                aContent.forEach(function (oControl) {
                    oShell.addItem(oControl);
                });
            }
            oItem.addContent(oShell);
            return oItem;
        }
    };
});
