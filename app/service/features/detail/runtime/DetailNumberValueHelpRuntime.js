sap.ui.define([
    "sap/m/SelectDialog",
    "sap/m/StandardListItem",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (SelectDialog, StandardListItem, Filter, FilterOperator, ModelStateRuntime, ModelContracts, DetailFieldContracts, JsRuntime) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MASTER_DATA_MODEL = MODELS.MASTER_DATA;
    var SELECTED_MODEL = MODELS.SELECTED;
    var AUTOSAVE_FIELDS = DetailFieldContracts.AUTOSAVE_FIELDS;
    var TEXT_PATHS = DetailFieldContracts.TEXT_PATHS;
    var VALUE_HELP_DIALOGS = DetailFieldContracts.VALUE_HELP_DIALOGS;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function resolveSelectChangeValue(oEvent) {
        return String((oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey && oEvent.getParameter("selectedItem").getKey()) || "").trim();
    }

    function resolveSelectChangeText(oEvent) {
        var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        if (oSelectedItem && typeof oSelectedItem.getText === TYPE_FUNCTION) {
            return String(oSelectedItem.getText() || "").trim();
        }
        return "";
    }

    function autosaveSelectChange(oController, oEvent, sTextPath, sFieldKey, mHooks) {
        var sValue = resolveSelectChangeValue(oEvent);
        ModelStateRuntime.write(oController, SELECTED_MODEL, sTextPath, resolveSelectChangeText(oEvent));
        return mHooks.autosave({
            field: sFieldKey,
            value: sValue
        });
    }

    function getDialogModelConfig(sKind) {
        if (sKind === "barrier") {
            return {
                autosaveField: AUTOSAVE_FIELDS.BARRIERS_NUMBER,
                dialogKey: VALUE_HELP_DIALOGS.BARRIERS_NUMBER,
                inputPath: "/basic/BARRIERS_NUMBER",
                itemsPath: "/barriersNumbers",
                textPath: TEXT_PATHS.BARRIERS_NUMBER
            };
        }
        return {
            autosaveField: AUTOSAVE_FIELDS.CHECKS_NUMBER,
            dialogKey: VALUE_HELP_DIALOGS.CHECKS_NUMBER,
            inputPath: "/basic/CHECKS_NUMBER",
            itemsPath: "/checksNumbers",
            textPath: TEXT_PATHS.CHECKS_NUMBER
        };
    }

    function getResourceText(oController, sKey, sFallback) {
        if (oController && typeof oController.getText === TYPE_FUNCTION) {
            return oController.getText(sKey, [], sFallback);
        }
        if (oController && typeof oController.getResourceBundle === TYPE_FUNCTION) {
            return oController.getResourceBundle().getText(sKey);
        }
        return sFallback || sKey;
    }

    function applyValueHelpSelection(oController, sKind, oSelectedItem, mHooks) {
        var mConfig = getDialogModelConfig(sKind);
        var oContext = oSelectedItem && oSelectedItem.getBindingContext && oSelectedItem.getBindingContext(MASTER_DATA_MODEL);
        var oData = oContext && oContext.getObject ? oContext.getObject() : {};
        var sValue = String(oData && oData.key || "").trim();
        var sText = String(oData && oData.text || oSelectedItem && oSelectedItem.getTitle && oSelectedItem.getTitle() || "").trim();
        ModelStateRuntime.write(oController, SELECTED_MODEL, mConfig.inputPath, sValue);
        ModelStateRuntime.write(oController, SELECTED_MODEL, mConfig.textPath, sText);
        return mHooks.autosave({
            field: mConfig.autosaveField,
            value: sValue
        });
    }

    function ensureNumberValueHelp(oController, sKind, mHooks) {
        var mConfig = getDialogModelConfig(sKind);
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        var oDialog = mHooks.getLazyDialog(mConfig.dialogKey);
        if (oDialog) {
            return Promise.resolve(oDialog);
        }
        oDialog = new SelectDialog(oView.createId(mConfig.dialogKey), {
            title: getResourceText(oController, sKind === "barrier" ? "barriersNumberLabel" : "checksNumberLabel", sKind === "barrier" ? "Barrier number" : "Check number"),
            noDataText: getResourceText(oController, "valueHelpNoData", "No data"),
            search: function (oEvent) {
                var sValue = String(oEvent.getParameter("value") || "").trim();
                var oBinding = oEvent.getSource().getBinding("items");
                if (!oBinding) {
                    return;
                }
                oBinding.filter(sValue ? [new Filter({
                    filters: [
                        new Filter("key", FilterOperator.Contains, sValue),
                        new Filter("text", FilterOperator.Contains, sValue)
                    ],
                    and: false
                })] : []);
            },
            liveChange: function (oEvent) {
                this.fireSearch({ value: oEvent.getParameter("value") });
            },
            confirm: function (oEvent) {
                var oSelectedItem = oEvent.getParameter("selectedItem");
                if (!oSelectedItem) {
                    return;
                }
                return applyValueHelpSelection(oController, sKind, oSelectedItem, mHooks);
            },
            cancel: function () {
                if (typeof oController._restoreDialogFocus === TYPE_FUNCTION) {
                    oController._restoreDialogFocus(mConfig.dialogKey);
                }
            }
        });
        oDialog[METHODS.ATTACH_AFTER_CLOSE](function () {
            if (typeof oController._restoreDialogFocus === TYPE_FUNCTION) {
                oController._restoreDialogFocus(mConfig.dialogKey);
            }
        });
        oDialog.setModel(oController.getModel(MASTER_DATA_MODEL), MASTER_DATA_MODEL);
        oDialog.bindAggregation("items", {
            path: MASTER_DATA_MODEL + ">" + mConfig.itemsPath,
            templateShareable: true,
            template: new StandardListItem({
                title: "{" + MASTER_DATA_MODEL + ">key}",
                description: "{" + MASTER_DATA_MODEL + ">text}"
            })
        });
        oView.addDependent(oDialog);
        mHooks.setLazyDialog(mConfig.dialogKey, oDialog);
        return Promise.resolve(oDialog);
    }

    function openNumberValueHelp(oController, oEvent, sKind, mHooks) {
        var mConfig = getDialogModelConfig(sKind);
        var oSource = oEvent && typeof oEvent.getSource === TYPE_FUNCTION && oEvent.getSource();
        mHooks.rememberDialogReturnFocus(mConfig.dialogKey, oSource);
        return ensureNumberValueHelp(oController, sKind, mHooks).then(function (oDialog) {
            oDialog.setRememberSelections(false);
            oDialog[METHODS.OPEN](String(ModelStateRuntime.read(oController, SELECTED_MODEL, mConfig.textPath, "")
                || ModelStateRuntime.read(oController, SELECTED_MODEL, mConfig.inputPath, "")));
        });
    }

    return {
        onBarriersNumberChange: function (oController, oEvent, mHooks) {
            return autosaveSelectChange(oController, oEvent, TEXT_PATHS.BARRIERS_NUMBER, AUTOSAVE_FIELDS.BARRIERS_NUMBER, mHooks);
        },
        onChecksNumberChange: function (oController, oEvent, mHooks) {
            return autosaveSelectChange(oController, oEvent, TEXT_PATHS.CHECKS_NUMBER, AUTOSAVE_FIELDS.CHECKS_NUMBER, mHooks);
        },
        onOpenBarriersNumberValueHelp: function (oController, oEvent, mHooks) {
            return openNumberValueHelp(oController, oEvent, "barrier", mHooks);
        },
        onOpenChecksNumberValueHelp: function (oController, oEvent, mHooks) {
            return openNumberValueHelp(oController, oEvent, "check", mHooks);
        },
        onLpcChange: function (oController, oEvent, mHooks) {
            return autosaveSelectChange(oController, oEvent, TEXT_PATHS.LPC, AUTOSAVE_FIELDS.LPC_KEY, mHooks);
        },
        onProfessionChange: function (oController, oEvent, mHooks) {
            return autosaveSelectChange(oController, oEvent, TEXT_PATHS.PROFESSION, AUTOSAVE_FIELDS.PROF_KEY, mHooks);
        }
    };
});
