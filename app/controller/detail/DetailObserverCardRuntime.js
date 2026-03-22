sap.ui.define([
    "sap/m/VBox",
    "sap/m/Text",
    "sap/m/Input",
    "sap/ui/core/ListItem",
    "sap/ui/core/CustomData",
    "sap/m/ObjectStatus",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts"
], function (VBox, Text, Input, CoreListItem, CustomData, ObjectStatus, ControlStyleRuntime, ControllerTextRuntime, WorkflowContracts) {
    "use strict";

    var LABEL_FALLBACKS = {
        personPernerLabel: "PERNER",
        personPositionLabel: "Position",
        personOrgUnitLabel: "Org unit"
    };

    function withStyleClasses(oControl, sClassNames) {
        return ControlStyleRuntime.enable(oControl, sClassNames);
    }

    function bindSuggestionItems(oInput, sPath) {
        oInput.bindAggregation("suggestionItems", {
            path: sPath,
            templateShareable: true,
            template: new CoreListItem({
                key: "{view>perner}",
                text: "{view>fullName}",
                additionalText: {
                    parts: [
                        { path: "view>perner" },
                        { path: "view>position" },
                        { path: "view>orgUnit" }
                    ],
                    formatter: function (sPerner, sPosition, sOrgUnit) {
                        return [sPerner, sPosition, sOrgUnit].filter(Boolean).join(" | ");
                    }
                }
            })
        });
        return oInput;
    }

    function createPrefixedStatus(oController, sLabelKey, sValuePath) {
        var sLabel = ControllerTextRuntime.getText(oController, sLabelKey, [], LABEL_FALLBACKS[sLabelKey] || sLabelKey);
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

    function bindReadVisibility(oControl) {
        if (!oControl || !oControl.bindProperty) {
            return oControl;
        }
        oControl.bindProperty("visible", {
            path: "state>/workflow/detail/editMode",
            formatter: function (sMode) { return sMode === WorkflowContracts.EDIT_MODES.READ; }
        });
        return oControl;
    }

    function bindEditVisibility(oControl) {
        if (!oControl || !oControl.bindProperty) {
            return oControl;
        }
        oControl.bindProperty("visible", {
            path: "state>/workflow/detail/editMode",
            formatter: function (sMode) { return sMode !== WorkflowContracts.EDIT_MODES.READ; }
        });
        return oControl;
    }

    return {
        createContent: function (oController, sTarget, mHooks) {
            var sUpper = sTarget.toUpperCase();
            var sInputPath = sTarget === "observer" ? "view>/observerInputValue" : "view>/observedInputValue";
            var sSuggestionsPath = sTarget === "observer" ? "view>/observerSuggestions" : "view>/observedSuggestions";
            var oBox = withStyleClasses(new VBox({ renderType: "Bare" }), "detailSemanticMetaBlock");
        var oReadText = createReadInfoText("{detail>/current/basic/" + sUpper + "_FULLNAME}");
            var oInput = new Input({
                value: "{" + sInputPath + "}",
                showSuggestion: "{= ${state>/workflow/detail/editMode} === 'EDIT' }",
                startSuggestion: 2,
                maxSuggestionWidth: "300%",
                filterSuggests: false,
                suggest: [oController.onPersonSuggest, oController],
                change: [oController.onPersonInputChange, oController],
                suggestionItemSelected: [oController.onPersonSuggestionSelected, oController]
            });
            var oHint = new Text({
                visible: "{= !!${view>/personSuggestHint} }"
            });

            withStyleClasses(oInput, "detailPersonSuggestInput");
            withStyleClasses(oHint, "detailInlineHintText detailInlineHintError") /* SAP-BP: detailInlineHintError replaces sapThemeNegativeText (SAP internal class) */;
            bindReadVisibility(oReadText);
            bindEditVisibility(oInput);
            oHint.bindProperty("text", {
                path: "view>/personSuggestHint",
                formatter: oController.formatI18nByKey.bind(oController)
            });
            oInput.addCustomData(new CustomData({ key: "target", value: sTarget, writeToDom: false }));
            bindSuggestionItems(oInput, sSuggestionsPath);

            oBox.addItem(oReadText);
            oBox.addItem(mHooks.wrapEditableField(oController, oInput, "basic." + sUpper + "_FULLNAME"));
            oBox.addItem(oHint);
        oBox.addItem(createPrefixedStatus(oController, "personPernerLabel", "detail>/current/basic/" + sUpper + "_PERNER"));
        oBox.addItem(createPrefixedStatus(oController, "personPositionLabel", "detail>/current/basic/" + sUpper + "_POSITION"));
        oBox.addItem(createPrefixedStatus(oController, "personOrgUnitLabel", "detail>/current/basic/" + sUpper + "_ORGUNIT"));
            return oBox;
        }
    };
});
