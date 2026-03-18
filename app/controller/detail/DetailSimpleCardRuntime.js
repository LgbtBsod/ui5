sap.ui.define([
    "sap/m/VBox",
    "sap/m/Text",
    "sap/m/Input",
    "sap/m/DatePicker",
    "sap/m/TimePicker",
    "sap/m/Select",
    "sap/ui/core/Item",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (VBox, Text, Input, DatePicker, TimePicker, Select, CoreItem, ControlStyleRuntime, WorkflowContracts) {
    "use strict";

    function withStyleClasses(oControl, sClassNames) {
        return ControlStyleRuntime.enable(oControl, sClassNames);
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

    return {
        createContent: function (oController, sKey, mHooks) {
            var oReadText = bindReadVisibility(new Text());
            var oEditBox = bindEditVisibility(new VBox({ renderType: "Bare" }));

            withStyleClasses(oReadText, "infoCardValue");

            if (sKey === "datetime") {
                withStyleClasses(oEditBox, "detailDateTimeGrid");
                oEditBox.addItem(mHooks.wrapEditableField(oController, new DatePicker({
                    value: "{selected>/basic/date}",
                    displayFormat: "EEE, dd MMM yyyy",
                    valueFormat: "yyyy-MM-dd",
                    change: [oController.onRowValueChange, oController]
                }), "basic.date"));
                oEditBox.addItem(mHooks.wrapEditableField(oController, new TimePicker({
                    value: "{selected>/basic/time}",
                    displayFormat: "HH:mm",
                    valueFormat: "HH:mm",
                    change: [oController.onRowValueChange, oController]
                }), "basic.time"));
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(new Select({
                    selectedKey: "{selected>/basic/timezone}",
                    forceSelection: false,
                    change: [oController.onRowValueChange, oController]
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
                oEditBox.addItem(mHooks.wrapEditableField(oController, new Input({
                    value: "{selected>/basic/equipment}",
                    change: [oController.onRowValueChange, oController]
                }), "basic.equipment"));
                oReadText.bindProperty("text", {
                    path: "selected>/basic/equipment",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            } else if (sKey === "location") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, withStyleClasses(new Input({
                    value: "{selected>/basic/LOCATION_NAME}",
                    showValueHelp: true,
                    valueHelpOnly: true,
                    valueHelpRequest: [oController.onOpenLocationValueHelp, oController],
                    placeholder: "{i18n>locationValueHelpPlaceholder}"
                }), "detailLocationInput"), "basic.LOCATION_KEY"));
                oEditBox.addItem(withStyleClasses(mHooks.createPrefixedStatus(oController, "locationCodeLabel", "selected>/basic/LOCATION_KEY"), "sapUiTinyMarginTop"));
                oReadText.bindProperty("text", {
                    parts: [
                        { path: "selected>/basic/LOCATION_NAME" },
                        { path: "selected>/basic/LOCATION_TEXT" }
                    ],
                    formatter: function (sName, sText) { return sName || sText || "-"; }
                });
            } else if (sKey === "lpc") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                    selectedKey: "{selected>/basic/LPC_KEY}",
                    change: [oController.onLpcChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/lpc"), "basic.LPC_KEY"));
                oReadText.bindProperty("text", {
                    path: "selected>/basic/LPC_TEXT",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            } else if (sKey === "profession") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                    selectedKey: "{selected>/basic/PROF_KEY}",
                    change: [oController.onProfessionChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/professions"), "basic.PROF_KEY"));
                oReadText.bindProperty("text", {
                    path: "selected>/basic/PROF_TEXT",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            } else if (sKey === "criteriaNumbers") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                    selectedKey: "{selected>/basic/CHECKS_NUMBER}",
                    change: [oController.onChecksNumberChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/checksNumbers"), "basic.CHECKS_NUMBER"));
                oEditBox.addItem(withStyleClasses(mHooks.createPrefixedStatus(oController, "checksNumberLabel", "selected>/basic/CHECKS_NUMBER"), "sapUiTinyMarginTop"));
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
                    selectedKey: "{selected>/basic/BARRIERS_NUMBER}",
                    change: [oController.onBarriersNumberChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/barriersNumbers"), "basic.BARRIERS_NUMBER"));
                oEditBox.addItem(withStyleClasses(mHooks.createPrefixedStatus(oController, "barriersNumberLabel", "selected>/basic/BARRIERS_NUMBER"), "sapUiTinyMarginTop"));
                oReadText.bindProperty("text", {
                    parts: [
                        { path: "selected>/basic/CHECKS_NUMBER_TEXT" },
                        { path: "selected>/basic/CHECKS_NUMBER" },
                        { path: "selected>/basic/BARRIERS_NUMBER_TEXT" },
                        { path: "selected>/basic/BARRIERS_NUMBER" }
                    ],
                    formatter: function (sChecksText, sChecksCode, sBarriersText, sBarriersCode) {
                        var sChecks = sChecksText || sChecksCode || "-";
                        var sBarriers = sBarriersText || sBarriersCode || "-";
                        return sChecks + " | " + sBarriers;
                    }
                });
            }

            return [oReadText, oEditBox];
        }
    };
});
