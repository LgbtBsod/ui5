sap.ui.define([
    "sap/m/VBox",
    "sap/m/Text",
    "sap/m/Input",
    "sap/m/DatePicker",
    "sap/m/TimePicker",
    "sap/m/Select",
    "sap/ui/core/Item",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants"
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
            value: "{detail>/current/basic/date}",
                    displayFormat: "EEE, dd MMM yyyy",
                    valueFormat: "yyyy-MM-dd",
                    change: [oController.onRowValueChange, oController]
                }), "basic.date"));
                oEditBox.addItem(mHooks.wrapEditableField(oController, new TimePicker({
            value: "{detail>/current/basic/time}",
                    displayFormat: "HH:mm",
                    valueFormat: "HH:mm",
                    change: [oController.onRowValueChange, oController]
                }), "basic.time"));
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(new Select({
            selectedKey: "{detail>/current/basic/timezone}",
                    forceSelection: false,
                    change: [oController.onRowValueChange, oController]
                }), "masterData>/timezones"), "basic.timezone"));
                oReadText.bindProperty("text", {
                    parts: [
                        { path: "view>key" },
                { path: "detail>/current/basic/date" },
                { path: "detail>/current/basic/time" },
                { path: "detail>/current/basic/timezone" },
                { path: "detail>/current/basic/equipment" },
                { path: "detail>/current/basic/OBSERVER_FULLNAME" },
                { path: "detail>/current/basic/OBSERVED_FULLNAME" },
                { path: "detail>/current/basic/LOCATION_NAME" },
                { path: "detail>/current/basic/LOCATION_TEXT" },
                { path: "detail>/current/basic/LPC_TEXT" },
                { path: "detail>/current/basic/PROF_TEXT" }
                    ],
                    formatter: oController.formatInfoCardValue.bind(oController)
                });
            } else if (sKey === "equipment") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, new Input({
            value: "{detail>/current/basic/equipment}",
                    change: [oController.onRowValueChange, oController]
                }), "basic.equipment"));
                oReadText.bindProperty("text", {
            path: "detail>/current/basic/equipment",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            } else if (sKey === "location") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, withStyleClasses(new Input({
            value: "{detail>/current/basic/LOCATION_NAME}",
                    showValueHelp: true,
                    valueHelpOnly: true,
                    valueHelpRequest: [oController.onOpenLocationValueHelp, oController],
                    placeholder: "{i18n>locationValueHelpPlaceholder}"
                }), "detailLocationInput"), "basic.LOCATION_KEY"));
            oEditBox.addItem(withStyleClasses(mHooks.createPrefixedStatus(oController, "locationCodeLabel", "detail>/current/basic/LOCATION_KEY"), "detailInlineMetaStatus"));
                oReadText.bindProperty("text", {
                    parts: [
                { path: "detail>/current/basic/LOCATION_NAME" },
                { path: "detail>/current/basic/LOCATION_TEXT" }
                    ],
                    formatter: function (sName, sText) { return sName || sText || "-"; }
                });
            } else if (sKey === "lpc") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
            selectedKey: "{detail>/current/basic/LPC_KEY}",
                    change: [oController.onLpcChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/lpc"), "basic.LPC_KEY"));
                oReadText.bindProperty("text", {
            path: "detail>/current/basic/LPC_TEXT",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            } else if (sKey === "profession") {
                oEditBox.addItem(mHooks.wrapEditableField(oController, bindSelectItems(withStyleClasses(new Select({
            selectedKey: "{detail>/current/basic/PROF_KEY}",
                    change: [oController.onProfessionChange, oController],
                    forceSelection: false
                }), "detailDictionarySelect"), "masterData>/professions"), "basic.PROF_KEY"));
                oReadText.bindProperty("text", {
            path: "detail>/current/basic/PROF_TEXT",
                    formatter: function (sValue) { return sValue || "-"; }
                });
            }

            return [oReadText, oEditBox];
        }
    };
});
