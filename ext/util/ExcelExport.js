sap.ui.define([
        "sap/ui/export/Spreadsheet",
        "sap/ui/export/library",
        "sap/ui/model/FilterType",
        "sap/m/MessageToast",
        "sap/m/MessageBox",
        "sap/m/Button",
        "./Constants",
        "./I18n"
], function (Spreadsheet, exportLibrary, FilterType, MessageToast, MessageBox, Button, Constants, I18n) {
        "use strict";

        const EdmType = exportLibrary.EdmType;

        // SSOT-предупреждение: 5 из 7 подписей (DocId/Date/LpcText/ProfText/
        // LocationName) дублируют Label из annotations.xml UI.LineItem для
        // ZCheckService.CheckRoot — при правке annotations.xml сверяй и эти
        // i18n-ключи (exportColDocId и т.д.). Полностью убрать дублирование через
        // deriveColumnsFromLineItem() ниже нельзя: SuccessRateChecks/
        // SuccessRateBarriers в LineItem представлены как UI.DataFieldForAnnotation
        // (@UI.DataPoint), у них нет Value.Path — deriveColumnsFromLineItem их
        // физически не увидит (фильтр "oRecord.Value && oRecord.Value.Path"), а
        // это единственные два дополнительных, содержательных столбца, которые
        // buildDefaultColumns добавляет сверх LineItem-полей.
        function buildDefaultColumns() {
                return [
                        { label: I18n.getText("exportColDocId"),       property: "DocId",               type: EdmType.String },
                        { label: I18n.getText("exportColCheckDate"),   property: "Date",                type: EdmType.Date },
                        { label: I18n.getText("exportColPkLevel"),     property: "LpcText",             type: EdmType.String },
                        { label: I18n.getText("exportColProfession"),  property: "ProfText",            type: EdmType.String },
                        { label: I18n.getText("exportColLocation"),    property: "LocationName",        type: EdmType.String },
                        { label: I18n.getText("exportColChecksRate"),  property: "SuccessRateChecks",   type: EdmType.Number },
                        { label: I18n.getText("exportColBarriersRate"), property: "SuccessRateBarriers", type: EdmType.Number }
                ];
        }

        function buildSheet(aRows, aColumns) {
                if (!aRows.length) {
                        MessageToast.show(I18n.getText("exportNoDataMsg"));
                        return;
                }
                const oSheet = new Spreadsheet({
                        workbook: { columns: aColumns || buildDefaultColumns() },
                        dataSource: aRows,
                        fileName: I18n.getText("exportFileName")
                });
                oSheet.build().finally(() => oSheet.destroy());
        }

        function extractRowsFromBinding(aContexts) {
                if (!aContexts || !aContexts.length) {
                        return [];
                }
                return aContexts.map((oCtx) => oCtx.getObject()).filter(Boolean);
        }

        // Edm.* -> sap.ui.export.library.EdmType. Раньше все LineItem-колонки
        // экспортировались как EdmType.String независимо от реального типа
        // OData-свойства — Date/Decimal-поля (напр. CheckRoot/Date) теряли
        // корректное типирование ячейки в Excel (сортировка/формат как текст,
        // не как дата/число). Список сознательно неполный (только типы, реально
        // встречающиеся в metadata.xml этого сервиса) — расширять по мере
        // появления новых типов, не "на все случаи жизни" (YAGNI).
        const EDM_TYPE_MAP = {
                "Edm.DateTime": EdmType.Date,
                "Edm.Time": EdmType.String,
                "Edm.Decimal": EdmType.Number,
                "Edm.Int16": EdmType.Number,
                "Edm.Int32": EdmType.Number,
                "Edm.Int64": EdmType.Number,
                "Edm.Double": EdmType.Number,
                "Edm.Single": EdmType.Number,
                "Edm.Boolean": EdmType.Boolean
        };

        function resolveEdmType(oEntityType, sPropertyPath) {
                const oProperty = (oEntityType.property || []).find((oProp) => oProp.name === sPropertyPath);
                const sEdmType = oProperty && oProperty.type;
                return EDM_TYPE_MAP[sEdmType] || EdmType.String;
        }

        // Колонки экспорта берутся из UI.LineItem той же сущности, что уже
        // рендерит SmartTable — чтобы не дублировать список полей/лейблов
        // вручную и не разойтись с annotations.xml (SSOT).
        function deriveColumnsFromLineItem(oSmartTable) {
                const oModel = oSmartTable.getModel();
                const sEntitySet = oSmartTable.getEntitySet && oSmartTable.getEntitySet();
                if (!oModel || !sEntitySet) {
                        return null;
                }
                const oMetaModel = oModel.getMetaModel();
                const oODataEntitySet = oMetaModel.getODataEntitySet(sEntitySet);
                const oEntityType = oODataEntitySet && oMetaModel.getODataEntityType(oODataEntitySet.entityType);
                const oLineItem = oEntityType && oEntityType["com.sap.vocabularies.UI.v1.LineItem"];
                if (!oLineItem) {
                        return null;
                }
                return oLineItem
                        .filter((oRecord) => oRecord.Value && oRecord.Value.Path)
                        .map((oRecord) => ({
                                label: (oRecord.Label && oRecord.Label.String) || oRecord.Value.Path,
                                property: oRecord.Value.Path,
                                type: resolveEdmType(oEntityType, oRecord.Value.Path)
                        }));
        }

        class ExcelExport {
                static attachButton(oSmartTable, oToolbar) {
                        oToolbar.addContent(new Button({
                                icon: "sap-icon://excel-attachment",
                                tooltip: I18n.getText("exportTooltip"),
                                press: () => ExcelExport.run(oSmartTable, deriveColumnsFromLineItem(oSmartTable))
                        }).addStyleClass(ExcelExport.EXPORT_BUTTON_CLASS));
                }

                // Экспорт из клиентского биндинга без OData roundtrip, если данных
                // достаточно; иначе — серверный запрос с $top=ROW_LIMIT.
                static run(oSmartTable, aColumns) {
                        const oTable = oSmartTable.getTable && oSmartTable.getTable();
                        const oBinding = oTable && oTable.getBinding && oTable.getBinding("items");
                        const oModel = oSmartTable.getModel();

                        if (!oBinding || !oModel) {
                                MessageToast.show(I18n.getText("exportTableNotReadyMsg"));
                                return;
                        }

                        const aContexts = oBinding.getContexts ? oBinding.getContexts() : [];
                        const aClientRows = extractRowsFromBinding(aContexts);

                        const iLengthInfo = oBinding.getLength ? oBinding.getLength() : aClientRows.length;
                        const bIsClientDataSufficient = aClientRows.length > 0 && (
                                aClientRows.length >= iLengthInfo ||
                                aClientRows.length >= Constants.EXPORT.ROW_LIMIT
                        );

                        if (bIsClientDataSufficient) {
                                if (iLengthInfo > aClientRows.length && iLengthInfo > Constants.EXPORT.ROW_LIMIT) {
                                        MessageBox.warning(
                                                I18n.getText("exportTruncatedWarningMsg", [aClientRows.length, iLengthInfo])
                                        );
                                }
                                buildSheet(aClientRows, aColumns);
                                return;
                        }

                        oModel.read(oBinding.getPath(), {
                                context: oBinding.getContext(),
                                filters: oBinding.getFilters(FilterType.Application).concat(oBinding.getFilters(FilterType.Control)),
                                sorters: oBinding.aSorters || [],
                                urlParameters: {
                                        "$top": String(Constants.EXPORT.ROW_LIMIT),
                                        "$inlinecount": "allpages"
                                },
                                success: (oData) => {
                                        const iTotal = parseInt(oData.__count, 10) || 0;
                                        const aServerRows = oData.results || [];
                                        if (iTotal > aServerRows.length) {
                                                MessageBox.warning(I18n.getText("exportTruncatedWarningMsg", [aServerRows.length, iTotal]));
                                        }
                                        buildSheet(aServerRows, aColumns);
                                },
                                error: () => MessageToast.show(I18n.getText("exportFailedMsg"))
                        });
                }
        }
        ExcelExport.EXPORT_BUTTON_CLASS = "pcLiteExportButton";

        return ExcelExport;
});
