sap.ui.define([
    "sap/f/GridListItem",
    "sap/m/VBox",
    "sap/m/HBox",
    "sap/m/Text",
    "sap/m/Input",
    "sap/m/DatePicker",
    "sap/m/TimePicker",
    "sap/m/Select",
    "sap/m/ObjectStatus",
    "sap/m/Button",
    "sap/ui/core/Item",
    "sap/ui/core/CustomData",
    "checklist/app/service/framework/ControlStyleRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/service/framework/NavigationIntentService",
    "checklist/app/service/framework/RootIdRuntime",
    "checklist/app/controller/base/ControllerTextRuntime",
    "checklist/app/service/framework/ModelStateRuntime"
], function (GridListItem, VBox, HBox, Text, Input, DatePicker, TimePicker, Select, ObjectStatus, Button, CoreItem, CustomData, ControlStyleRuntime, LayoutStateRuntime, NavigationIntentService, RootIdRuntime, ControllerTextRuntime, ModelStateRuntime) {
    "use strict";

    var CARD_REQUIRED_KEYS = {
        datetime: ["basic.date", "basic.time", "basic.timezone"],
        equipment: ["basic.equipment"],
        observer: ["basic.OBSERVER_FULLNAME"],
        observed: ["basic.OBSERVED_FULLNAME"],
        location: ["basic.LOCATION_KEY"],
        lpc: ["basic.LPC_KEY"],
        profession: ["basic.PROF_KEY"]
    };

    function withStyleClasses(oControl, sClassNames) {
        return ControlStyleRuntime.enable(oControl, sClassNames);
    }

    function resolveValidationKey(sValidationKey) {
        return toValidationKey(sValidationKey);
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

    function normalizeValidationPath(sPath) {
        return "/" + String(sPath || "").replace(/^\//, "");
    }

    function toValidationKey(sPath) {
        return normalizeValidationPath(sPath)
            .replace(/^\//, "")
            .replace(/\//g, ".");
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

    function bindSelectItems(oSelect, sPath) {
        oSelect.bindAggregation("items", {
            path: sPath,
            templateShareable: true,
            template: new CoreItem({
                key: "{masterData>key}",
                text: "{masterData>text}"
            })
        });
        return oSelect;
    }

    function bindSuggestionItems(oInput, sPath, oController) {
        oInput.bindAggregation("suggestionItems", {
            path: sPath,
            templateShareable: true,
            template: new CoreItem({
                key: "{view>perner}",
                text: {
                    parts: [
                        { path: "view>fullName" },
                        { path: "view>position" },
                        { path: "view>perner" },
                        { path: "view>orgUnit" }
                    ],
                    formatter: oController.formatPersonSuggestion.bind(oController)
                }
            })
        });
        return oInput;
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

    function createReadInfoText(sTextBinding) {
        return withStyleClasses(new Text({ text: sTextBinding, renderWhitespace: true }), "infoCardValue");
    }

    function createInfoCardWrapper(sId, oContext) {
        var oItem = new GridListItem(sId, {
            customData: [
                new CustomData({
                    key: "cardkey",
                    value: "{view>key}",
                    writeToDom: true
                })
            ]
        });
        withStyleClasses(oItem, "infoCardGridItem");
        oItem.setBindingContext(oContext, "view");
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

    function bindReadVisibility(oControl) {
        if (!oControl || !oControl.bindProperty) {
            return oControl;
        }
        oControl.bindProperty("visible", {
            path: "state>/mode",
            formatter: function (sMode) { return sMode === "READ"; }
        });
        return oControl;
    }

    function bindEditVisibility(oControl) {
        if (!oControl || !oControl.bindProperty) {
            return oControl;
        }
        oControl.bindProperty("visible", {
            path: "state>/mode",
            formatter: function (sMode) { return sMode !== "READ"; }
        });
        return oControl;
    }

    function createObserverCardContent(oController, sTarget) {
        var sUpper = sTarget.toUpperCase();
        var sInputPath = sTarget === "observer" ? "view>/observerInputValue" : "view>/observedInputValue";
        var oBox = withStyleClasses(new VBox({ renderType: "Bare" }), "detailSemanticMetaBlock");
        var oReadText = createReadInfoText("{selected>/basic/" + sUpper + "_FULLNAME}");
        var oInput = new Input({
            value: "{" + sInputPath + "}",
            showSuggestion: "{= ${state>/mode} === 'EDIT' }",
            startSuggestion: 1,
            maxSuggestionWidth: "300%",
            filterSuggests: false,
            suggest: [oController.onPersonSuggest, oController],
            change: [oController.onPersonInputChange, oController],
            suggestionItemSelected: [oController.onPersonSuggestionSelected, oController]
        });
        var oHint = new Text({
            visible: "{= !!${view>/personSuggestHint} }"
        });
        var sSuggestionsPath = sTarget === "observer" ? "view>/observerSuggestions" : "view>/observedSuggestions";

        withStyleClasses(oInput, "detailPersonSuggestInput");
        withStyleClasses(oHint, "sapUiTinyMarginTop sapThemeNegativeText");

        bindReadVisibility(oReadText);
        bindEditVisibility(oInput);
        oHint.bindProperty("text", {
            path: "view>/personSuggestHint",
            formatter: oController.formatI18nByKey.bind(oController)
        });
        oInput.addCustomData(new CustomData({ key: "target", value: sTarget, writeToDom: false }));
        bindSuggestionItems(oInput, sSuggestionsPath, oController);

        oBox.addItem(oReadText);
        oBox.addItem(wrapEditableField(oController, oInput, "basic." + sUpper + "_FULLNAME"));
        oBox.addItem(oHint);
        oBox.addItem(createPrefixedStatus(oController, "personPernerLabel", "selected>/basic/" + sUpper + "_PERNER"));
        oBox.addItem(createPrefixedStatus(oController, "personPositionLabel", "selected>/basic/" + sUpper + "_POSITION"));
        oBox.addItem(createPrefixedStatus(oController, "personOrgUnitLabel", "selected>/basic/" + sUpper + "_ORGUNIT"));
        return oBox;
    }

    function createSimpleEditCardContent(oController, sKey) {
        var oReadText = bindReadVisibility(new Text());
        var oEditBox = bindEditVisibility(new VBox({ renderType: "Bare" }));

        withStyleClasses(oReadText, "infoCardValue");

        if (sKey === "datetime") {
            withStyleClasses(oEditBox, "detailDateTimeGrid");
            oEditBox.addItem(wrapEditableField(oController, new DatePicker({
                value: "{selected>/basic/date}",
                displayFormat: "EEE, dd MMM yyyy",
                valueFormat: "yyyy-MM-dd"
            }), "basic.date"));
            oEditBox.addItem(wrapEditableField(oController, new TimePicker({
                value: "{selected>/basic/time}",
                displayFormat: "HH:mm",
                valueFormat: "HH:mm"
            }), "basic.time"));
            oEditBox.addItem(wrapEditableField(oController, bindSelectItems(new Select({
                selectedKey: "{selected>/basic/timezone}",
                forceSelection: false
            }), "masterData>/timezones"), "basic.timezone"));
            oReadText.bindProperty("text", {
                parts: [
                    { path: "view>key" },
                    { path: "selected>/basic/date" },
                    { path: "selected>/basic/time" },
                    { path: "selected>/basic/timezone" },
                    { path: "selected>/basic/equipment" },
                    { path: "selected>/basic/OBSERVER_FULLNAME" },
                    { path: "selected>/basic/OBSERVED_FULLNAME" },
                    { path: "selected>/basic/LOCATION_NAME" },
                    { path: "selected>/basic/LOCATION_TEXT" },
                    { path: "selected>/basic/LPC_TEXT" },
                    { path: "selected>/basic/PROF_TEXT" }
                ],
                formatter: oController.formatInfoCardValue.bind(oController)
            });
        } else if (sKey === "equipment") {
            oEditBox.addItem(wrapEditableField(oController, new Input({
                value: "{selected>/basic/equipment}"
            }), "basic.equipment"));
            oReadText.bindProperty("text", {
                path: "selected>/basic/equipment",
                formatter: function (sValue) { return sValue || "-"; }
            });
        } else if (sKey === "location") {
            oEditBox.addItem(wrapEditableField(oController, withStyleClasses(new Input({
                value: "{selected>/basic/LOCATION_NAME}",
                showValueHelp: true,
                valueHelpOnly: true,
                valueHelpRequest: [oController.onOpenLocationValueHelp, oController],
                placeholder: "{i18n>locationValueHelpPlaceholder}"
            }), "detailLocationInput"), "basic.LOCATION_KEY"));
            oEditBox.addItem(withStyleClasses(createPrefixedStatus(oController, "locationCodeLabel", "selected>/basic/LOCATION_KEY"), "sapUiTinyMarginTop"));
            oReadText.bindProperty("text", {
                parts: [
                    { path: "selected>/basic/LOCATION_NAME" },
                    { path: "selected>/basic/LOCATION_TEXT" }
                ],
                formatter: function (sName, sText) { return sName || sText || "-"; }
            });
        } else if (sKey === "lpc") {
            oEditBox.addItem(wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                selectedKey: "{selected>/basic/LPC_KEY}",
                change: [oController.onLpcChange, oController],
                forceSelection: false
            }), "detailDictionarySelect"), "masterData>/lpc"), "basic.LPC_KEY"));
            oReadText.bindProperty("text", {
                path: "selected>/basic/LPC_TEXT",
                formatter: function (sValue) { return sValue || "-"; }
            });
        } else if (sKey === "profession") {
            oEditBox.addItem(wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                selectedKey: "{selected>/basic/PROF_KEY}",
                change: [oController.onProfessionChange, oController],
                forceSelection: false
            }), "detailDictionarySelect"), "masterData>/professions"), "basic.PROF_KEY"));
            oReadText.bindProperty("text", {
                path: "selected>/basic/PROF_TEXT",
                formatter: function (sValue) { return sValue || "-"; }
            });
        }

        return [oReadText, oEditBox];
    }

    function buildInfoCard(oController, sId, oContext) {
        var sKey = String(oContext && oContext.getProperty && oContext.getProperty("key") || "");
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
            path: "state>/mode",
            formatter: function (sMode) {
                return sMode !== "READ";
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
        if (sKey === "observer" || sKey === "observed") {
            oShell.addItem(createObserverCardContent(oController, sKey));
        } else {
            aContent = createSimpleEditCardContent(oController, sKey);
            aContent.forEach(function (oControl) {
                oShell.addItem(oControl);
            });
        }
        oItem.addContent(oShell);
        return oItem;
    }

    function applyLayoutState(oController, vLayout, mOptions) {
        var oRouter = oController.getRouter && oController.getRouter();
        var bSyncRoute = !mOptions || mOptions.syncRoute !== false;
        var sRootId;
        var sLayout;
        if (!oController.getModel("state")) {
            return;
        }
        sRootId = RootIdRuntime.resolveFromController(oController);
        sLayout = LayoutStateRuntime.normalizeLayout(vLayout);
        ModelStateRuntime.write(oController, "state", "/layout", sLayout);
        if (!bSyncRoute || !oRouter) {
            return;
        }
        if (sLayout === "OneColumn") {
            NavigationIntentService.navigateToSearch(oController);
            return;
        }
        if (!sRootId) {
            return;
        }
        NavigationIntentService.navigateToDetail(oController, sRootId, sLayout);
    }

    return {
        applyLayoutState: applyLayoutState,
        buildInfoCard: buildInfoCard
    };
});

