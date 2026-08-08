sap.ui.define([
        "sap/ui/model/Filter",
        "sap/ui/model/FilterOperator",
        "sap/ui/model/Sorter",
        "sap/ui/model/json/JSONModel",
        "sap/m/Dialog",
        "sap/m/Button",
        "sap/m/SearchField",
        "sap/m/Table",
        "sap/m/Column",
        "sap/m/ColumnListItem",
        "sap/m/Text",
        "./DialogLifecycle",
        "./I18n"
], function (Filter, FilterOperator, Sorter, JSONModel, Dialog, Button, SearchField, Table, Column, ColumnListItem, Text,
        DialogLifecycle, I18n) {
        "use strict";

        // Справочники (CheckTypes/BarrierTypes/CheckResults) называют поля по
        // смыслу сущности, без общего code/text/category — пикер принимает имена
        // полей через oFields, а не хардкодит их.
        //
        // [Аудит W2.2, пересмотрено] Раньше — sap.m.SelectDialog с ручной
        // группировкой секциями (Sorter + groupHeaderFactory/GroupHeaderListItem).
        // SelectDialog внутри строит sap.m.List (не Table) и не умеет показывать
        // произвольное свойство как отдельную колонку — только группы-секции
        // или текст в поле "info" одной строки. Категория теперь — реальная
        // колонка таблицы (sap.m.Table + Column + ColumnListItem), без
        // группировки/сортировки в секции. Данные (aItems) по-прежнему приходят
        // уже отфильтрованными по PkLevel вызывающей стороной (SubTableCrud) —
        // сама таблица данные не запрашивает.
        function buildSearchFilters(oFields, bFlat, sValue) {
                if (!sValue) {
                        return [];
                }
                return [bFlat
                        ? new Filter(oFields.text, FilterOperator.Contains, sValue)
                        : new Filter({
                                filters: [
                                        new Filter(oFields.text, FilterOperator.Contains, sValue),
                                        new Filter(oFields.category, FilterOperator.Contains, sValue)
                                ],
                                and: false
                        })];
        }

        class TypePicker {
                /**
                 * @param {string} sLabel
                 * @param {object[]} aItems
                 * @param {function} fnOnSelect - (sCode, sText) => void
                 * @param {object} oOptions
                 * @param {object} oOptions.fields - { code, text, category? }
                 * @param {boolean} [oOptions.flat] - без колонки "Категория" (для плоских справочников, напр. CheckResults)
                 */
                static open(sLabel, aItems, fnOnSelect, oOptions) {
                        const oFields = oOptions.fields;
                        const bFlat = !!oOptions.flat;
                        const oPickerModel = new JSONModel({ items: aItems });
                        const destroyAll = () => DialogLifecycle.destroyWithModels(oDialog, [oPickerModel]);

                        const oTable = new Table({
                                mode: "SingleSelectLeft",
                                growing: true,
                                growingThreshold: 50,
                                noDataText: I18n.getText("noDataFoundText"),
                                columns: (bFlat ? [] : [
                                        new Column({ header: new Text({ text: I18n.getText("typePickerCategoryColumnLabel") }), width: "40%" })
                                ]).concat([
                                        new Column({ header: new Text({ text: sLabel }) })
                                ])
                        });
                        oTable.bindItems({
                                path: "/items",
                                sorter: (bFlat ? [] : [new Sorter(oFields.category, false)]).concat([new Sorter(oFields.text, false)]),
                                template: new ColumnListItem({
                                        type: "Active",
                                        cells: (bFlat ? [] : [new Text({ text: `{${oFields.category}}` })])
                                                .concat([new Text({ text: `{${oFields.text}}` })])
                                })
                        });

                        const fnApplySearch = (sValue) => oTable.getBinding("items").filter(buildSearchFilters(oFields, bFlat, sValue));
                        const oSearchField = new SearchField({
                                placeholder: I18n.getText("typePickerSearchPlaceholder"),
                                liveChange: (oEvent) => fnApplySearch(oEvent.getParameter("newValue")),
                                search: (oEvent) => fnApplySearch(oEvent.getParameter("query"))
                        });

                        const oDialog = new Dialog({
                                title: sLabel,
                                contentWidth: bFlat ? "20rem" : "32rem",
                                contentHeight: "28rem",
                                resizable: true,
                                verticalScrolling: false,
                                content: [oSearchField, oTable],
                                beginButton: new Button({
                                        text: I18n.getText("selectLabel"),
                                        type: "Emphasized",
                                        press: () => {
                                                const oSelected = oTable.getSelectedItem();
                                                if (oSelected) {
                                                        const oRow = oSelected.getBindingContext().getObject();
                                                        fnOnSelect(oRow[oFields.code], oRow[oFields.text]);
                                                }
                                                destroyAll();
                                        }
                                }),
                                endButton: new Button({
                                        text: I18n.getText("cancelLabel"),
                                        press: () => destroyAll()
                                })
                        });
                        oDialog.setModel(oPickerModel);
                        oDialog.open();
                }
        }

        return TypePicker;
});
