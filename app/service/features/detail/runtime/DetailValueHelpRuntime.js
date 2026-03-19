sap.ui.define([
    "sap/m/SelectDialog",
    "sap/m/StandardListItem",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts"
], function (SelectDialog, StandardListItem, Filter, FilterOperator, ModelStateRuntime, ModelContracts, OperationSourceContracts, DetailFieldContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MASTER_DATA_MODEL = MODELS.MASTER_DATA;
    var SELECTED_MODEL = MODELS.SELECTED;
    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;
    var AUTOSAVE_FIELDS = DetailFieldContracts.AUTOSAVE_FIELDS;
    var TEXT_PATHS = DetailFieldContracts.TEXT_PATHS;
    var VALUE_HELP_DIALOGS = DetailFieldContracts.VALUE_HELP_DIALOGS;
    var VIEW_PATHS = DetailFieldContracts.VIEW_PATHS;

    function resolveSelectChangeValue(oEvent) {
        return String((oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey && oEvent.getParameter("selectedItem").getKey()) || "").trim();
    }

    function resolveSelectChangeText(oEvent) {
        var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
        if (oSelectedItem && typeof oSelectedItem.getText === "function") {
            return String(oSelectedItem.getText() || "").trim();
        }
        return "";
    }

    function autosaveSelectChange(oController, oEvent, sTextPath, sFieldKey, mHooks) {
        var sValue = resolveSelectChangeValue(oEvent);
        ModelStateRuntime.write(oController, SELECTED_MODEL, sTextPath, resolveSelectChangeText(oEvent));
        mHooks.autosave({
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
        if (oController && typeof oController.getText === "function") {
            return oController.getText(sKey, [], sFallback);
        }
        if (oController && typeof oController.getResourceBundle === "function") {
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
        var oView = oController && oController.getView && oController.getView();
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
                if (typeof oController._restoreDialogFocus === "function") {
                    oController._restoreDialogFocus(mConfig.dialogKey);
                }
            }
        });
        oDialog.attachAfterClose(function () {
            if (typeof oController._restoreDialogFocus === "function") {
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

    return {
        closeLocationValueHelp: function (_oController, mHooks) {
            mHooks.clearSearchTimer();
            return Promise.resolve(mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.CLOSE })).finally(function () {
                mHooks.setViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, false);
            });
        },
        confirmLocationValueHelp: function (_oController, mHooks) {
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.CONFIRM });
            });
        },
        onBarriersNumberChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.BARRIERS_NUMBER, AUTOSAVE_FIELDS.BARRIERS_NUMBER, mHooks);
        },
        onChecksNumberChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.CHECKS_NUMBER, AUTOSAVE_FIELDS.CHECKS_NUMBER, mHooks);
        },
        onOpenBarriersNumberValueHelp: function (oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            mHooks.rememberDialogReturnFocus(VALUE_HELP_DIALOGS.BARRIERS_NUMBER, oSource);
            return ensureNumberValueHelp(oController, "barrier", mHooks).then(function (oDialog) {
                oDialog.setRememberSelections(false);
                oDialog.open(String(ModelStateRuntime.read(oController, SELECTED_MODEL, "/basic/BARRIERS_NUMBER_TEXT", "")
                    || ModelStateRuntime.read(oController, SELECTED_MODEL, "/basic/BARRIERS_NUMBER", "")));
            });
        },
        onOpenChecksNumberValueHelp: function (oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            mHooks.rememberDialogReturnFocus(VALUE_HELP_DIALOGS.CHECKS_NUMBER, oSource);
            return ensureNumberValueHelp(oController, "check", mHooks).then(function (oDialog) {
                oDialog.setRememberSelections(false);
                oDialog.open(String(ModelStateRuntime.read(oController, SELECTED_MODEL, "/basic/CHECKS_NUMBER_TEXT", "")
                    || ModelStateRuntime.read(oController, SELECTED_MODEL, "/basic/CHECKS_NUMBER", "")));
            });
        },
        onLocationTreeSelectionChange: function (_oController, oEvent, mHooks) {
            return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.TREE_SELECTION, event: oEvent });
        },
        onLocationValueHelpSearch: function (_oController, oEvent, mHooks) {
            var sValue = oEvent.getParameter("newValue");
            mHooks.clearSearchTimer();
            mHooks.restartSearchTimer(function () {
                Promise.resolve(mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                    return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.SEARCH, value: sValue });
                })).then(function () {
                    mHooks.scheduleTableSync();
                }).catch(function () {
                    return;
                });
            }, 180);
        },
        onLocationValueHelpSearchSubmit: function (_oController, oEvent, mHooks) {
            var sQuery = oEvent.getParameter("query");
            mHooks.clearSearchTimer();
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.SEARCH, value: sQuery });
            }).then(function () {
                mHooks.scheduleTableSync();
            });
        },
        onLpcChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.LPC, AUTOSAVE_FIELDS.LPC_KEY, mHooks);
        },
        onOpenLocationValueHelp: function (_oController, oEvent, mHooks) {
            mHooks.clearSearchTimer();
            mHooks.rememberDialogReturnFocus(VALUE_HELP_DIALOGS.LOCATION, oEvent && oEvent.getSource && oEvent.getSource());
            return mHooks.withViewFlag(VIEW_PATHS.LOCATION_VH_BUSY, function () {
                return mHooks.valueHelpLocation({ intent: DETAIL_SOURCES.OPEN });
            }).then(function (oResult) {
                mHooks.scheduleTableSync();
                return oResult;
            });
        },
        onPersonInputChange: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var sTarget = mHooks.personTargetFromSource(oSource);
            var sValue;
            if (!mHooks.isEditMode()) {
                return;
            }
            sValue = String((oEvent && oEvent.getParameter && oEvent.getParameter("value")) || "");
            if (mHooks.consumeSuggestionSelection(sTarget, sValue)) {
                return;
            }
            return mHooks.personSuggest({
                intent: DETAIL_SOURCES.MANUAL_CHANGE,
                value: sValue,
                target: sTarget
            });
        },
        onPersonSuggest: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            if (!mHooks.isEditMode()) {
                return;
            }
            return mHooks.personSuggest({
                term: oEvent.getParameter("suggestValue"),
                target: mHooks.personTargetFromSource(oSource)
            });
        },
        onPersonSuggestionSelected: function (_oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oSelectedItem = oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem");
            var sTarget = mHooks.personTargetFromSource(oSource);
            var sSelectedValue = "";
            if (!mHooks.isEditMode()) {
                return;
            }
            if (oSelectedItem && typeof oSelectedItem.getText === "function") {
                sSelectedValue = String(oSelectedItem.getText() || "");
            }
            mHooks.rememberSuggestionSelection(sTarget, sSelectedValue);
            return mHooks.personSuggest({
                intent: DETAIL_SOURCES.SELECTED,
                item: oSelectedItem,
                target: sTarget
            });
        },
        onProfessionChange: function (oController, oEvent, mHooks) {
            autosaveSelectChange(oController, oEvent, TEXT_PATHS.PROFESSION, AUTOSAVE_FIELDS.PROF_KEY, mHooks);
        }
    };
});
