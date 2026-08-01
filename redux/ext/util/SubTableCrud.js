sap.ui.define([
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"sap/m/Button",
	"sap/m/Dialog",
	"sap/m/Input",
	"sap/m/TextArea",
	"sap/m/Label",
	"sap/m/MessageToast",
	"sap/m/MessageBox",
	"sap/ui/layout/form/SimpleForm",
	"./Constants",
	"./ODataContextUtils",
	"./ODataUtils",
	"./TypePicker",
	"./LocalItemSuggest",
	"./I18n",
	"./ExcelExport",
	"./OnceRegistry",
	"./FioriElementsDom"
], function (Filter, FilterOperator, Button, Dialog, Input, TextArea, Label, MessageToast, MessageBox, SimpleForm,
	Constants, ODataContextUtils, ODataUtils, TypePicker, LocalItemSuggest, I18n, ExcelExport, OnceRegistry, FioriElementsDom) {
	"use strict";

	const F = Constants.FIELDS;
	const _oSetupTables = new OnceRegistry();

	const CSS_CLASSES = {
		ADD_BUTTON: "pcLiteAddEntryButton",
		DELETE_BUTTON: "pcLiteDeleteEntryButton",
		ADD_ROW_DIALOG: "pcLiteAddRowDialog"
	};

	class SubTableCrud {
		constructor() {
			this._oDoneSections = new Set();
		}

		// aSmartTables — опциональный снапшот из общего обхода дерева
		// (ObjectPageExtension._buildScanSnapshot); без него — свой findAggregatedObjects.
		scan(oView, oPageConfig, aSmartTables) {
			const aSectionKeys = Object.keys(oPageConfig.sections);
			if (aSectionKeys.every((sKey) => this._oDoneSections.has(sKey))) {
				return;
			}
			(aSmartTables || oView.findAggregatedObjects(true, (oElement) =>
				oElement.isA && oElement.isA("sap.ui.comp.smarttable.SmartTable")
			)).forEach((oElement) => this._setupIfNeeded(oView, oElement, oPageConfig));
		}

		_setupIfNeeded(oView, oSmartTable, oPageConfig) {
			if (!oSmartTable || !oSmartTable.isA || !oSmartTable.isA("sap.ui.comp.smarttable.SmartTable")) {
				return;
			}
			if (_oSetupTables.isMarked(oSmartTable)) {
				return;
			}
			// Граница токена: SmartTable ID вида "<component>--<facetId>::Table" —
			// без границы "CheckRowsSection" ложно матчился бы "CheckRowsSectionExtra".
			const sSectionKey = Object.keys(oPageConfig.sections).find((sKey) =>
				new RegExp(`(^|[^A-Za-z0-9])${sKey}($|[^A-Za-z0-9])`).test(oSmartTable.getId())
			);
			if (!sSectionKey) {
				return;
			}
			_oSetupTables.mark(oSmartTable);
			this._oDoneSections.add(sSectionKey);
			this._setupButtons(oView, oSmartTable, oPageConfig.sections[sSectionKey]);
		}

		_setupButtons(oView, oSmartTable, oConfig) {
			const oToolbar = FioriElementsDom.getSmartTableToolbar(oSmartTable);
			if (!oToolbar || !oToolbar.addContent) {
				return;
			}

			// [Проверено эмпирически] Нативные addEntry/deleteEntry для
			// composition-child SmartTable на Object Page в этой (non-draft,
			// classic sap.suite.ui.generic.template) конфигурации всегда
			// visible=false БЕЗ какого-либо биндинга (getBindingInfo("visible")
			// === null) — это не "не успели показать при editable=false",
			// framework решает это статически при построении дерева. Ни
			// setEditable, ни биндинг на ui>/editable это не переключит —
			// инлайн-создание строк для composition child в non-draft classic
			// template в принципе не поддерживается, отсюда и кастомный Dialog
			// ниже. Не убирать этот destroy без пересмотра решения целиком.
			const reAddEntry = new RegExp(`${Constants.ID_SUFFIXES.ADD_ENTRY}$`);
			const reDeleteEntry = new RegExp(`${Constants.ID_SUFFIXES.DELETE_ENTRY}$`);
			(oToolbar.getContent ? oToolbar.getContent() : []).slice().forEach((oControl) => {
				if (reAddEntry.test(oControl.getId()) || reDeleteEntry.test(oControl.getId())) {
					oToolbar.removeContent(oControl);
					oControl.destroy();
				}
			});

			const oInnerTable = oSmartTable.getTable && oSmartTable.getTable();
			if (oInnerTable && oInnerTable.bindProperty) {
				oInnerTable.bindProperty("mode", {
					path: `${Constants.UI_MODEL.NAME}>${Constants.UI_MODEL.EDITABLE_PATH}`,
					formatter: (bEditable) => (bEditable ? "MultiSelect" : "None")
				});
			}
			SubTableCrud._forceBlockPopin(oInnerTable);

			// initialNoDataText покрывает только состояние ДО первого запроса. После
			// первого dataReceived SmartTable подставляет свой framework-текст (ключ
			// не резолвится в 1.71 без ручной регистрации) и откатывается на общий
			// "Данные не найдены. Попробуйте изменить параметры фильтра." — вводящий
			// в заблуждение на просто пустом (не отфильтрованном) списке. Переустанавливаем
			// свой текст на inner-таблице (property "noDataText", не aggregation "noData" —
			// та только с 1.73+) при каждом dataReceived.
			const sNoRowsYetText = I18n.getText("noRowsYetText");
			oSmartTable.setInitialNoDataText(sNoRowsYetText);
			oSmartTable.attachDataReceived(() => {
				// Framework переустанавливает noDataText синхронно в своём же обработчике
				// dataReceived; порядок синхронных обработчиков не гарантирован, поэтому
				// откладываем на микротаск, чтобы точно выполниться после него.
				Promise.resolve().then(() => {
					if (oInnerTable && oInnerTable.setNoDataText) {
						oInnerTable.setNoDataText(sNoRowsYetText);
					}
				});
			});

			oToolbar.addContent(new Button({
				text: I18n.getText("addLabel"),
				type: "Transparent",
				visible: Constants.UI_MODEL.EDITABLE_BINDING,
				press: () => this._openAddRowDialog(oView, oSmartTable, oConfig)
			}).addStyleClass(CSS_CLASSES.ADD_BUTTON));

			oToolbar.addContent(new Button({
				text: I18n.getText("deleteLabel"),
				type: "Transparent",
				visible: Constants.UI_MODEL.EDITABLE_BINDING,
				press: () => this._onDeleteRow(oView, oSmartTable)
			}).addStyleClass(CSS_CLASSES.DELETE_BUTTON));

			ExcelExport.attachButton(oSmartTable, oToolbar);
		}

		// Общий Input с ValueHelp + Suggest для полей типа/результата (F4 и inline-suggest
		// используют один и тот же уже загруженный справочник, без второго HTTP-запроса).
		_createPickerInput(oConfig) {
			const oInput = new Input({
				showValueHelp: true,
				placeholder: oConfig.placeholder || I18n.getText("typeInputPlaceholder"),
				value: oConfig.value || "",
				valueHelpRequest: () => {
					TypePicker.open(oConfig.label, oConfig.items, (sCode, sText) => {
						oConfig.onPick(sCode, sText);
						oInput.setValue(sText);
					}, { fields: oConfig.fields, flat: !!oConfig.flat });
				},
				liveChange: () => {
					if (oConfig.selectedCodeHolder) {
						oConfig.selectedCodeHolder.code = null;
					}
				},
				suggestionItemSelected: (oEvent) => {
					const oItem = oEvent.getParameter("selectedItem");
					if (oItem && oConfig.selectedCodeHolder) {
						oConfig.selectedCodeHolder.code = oItem.getKey();
					}
				}
			});
			LocalItemSuggest.wire(oInput, oConfig.items, {
				code: oConfig.fields.code,
				text: oConfig.fields.text
			});
			return oInput;
		}

		_openAddRowDialog(oView, oSmartTable, oConfig) {
			const oHeaderContext = oView.getBindingContext();
			if (!oHeaderContext) {
				return;
			}
			if (ODataContextUtils.isTransient(oHeaderContext)) {
				MessageBox.warning(I18n.getText("saveHeaderFirstMsg"));
				return;
			}
			const sPkLevel = oHeaderContext.getProperty(F.LPC_KEY);
			const oModel = oView.getModel();

			const aTypeFilters = sPkLevel
				? [new Filter(F.PK_LEVELS, FilterOperator.Contains, String(sPkLevel))]
				: undefined;

			// $select — только поля, реально отрисовываемые в диалоге (code/text/
			// category у типа, code/text у результата); PkLevels участвует только
			// как $filter, в проекцию ответа не нужен.
			const sTypeSelect = [oConfig.typeFields.code, oConfig.typeFields.text, oConfig.typeFields.category]
				.filter(Boolean).join(",");
			const sResultSelect = [oConfig.resultFields.code, oConfig.resultFields.text]
				.filter(Boolean).join(",");

			Promise.all([
				this._readEntitySet(oModel, oConfig.typeEntitySet, aTypeFilters, sTypeSelect),
				this._readEntitySet(oModel, oConfig.resultEntitySet, undefined, sResultSelect)
			]).then(([aTypesRaw, aResultsRaw]) => {
				this._showAddRowDialog(oView, oSmartTable, oConfig, aTypesRaw, aResultsRaw);
			});
		}

		_readEntitySet(oModel, sEntitySet, aFilters, sSelect) {
			if (!sEntitySet) {
				return Promise.resolve([]);
			}
			return ODataUtils.readEntitySet(oModel, `/${sEntitySet}`, {
				filters: aFilters,
				urlParameters: sSelect ? { "$select": sSelect } : undefined,
				onError: () => MessageToast.show(I18n.getText("refDataLoadFailedMsg", [sEntitySet]))
			});
		}

		_showAddRowDialog(oView, oSmartTable, oConfig, aTypes, aResults) {
			const oTypeSelection = { code: null };
			const oResultSelection = { code: Constants.RESULT_CODES.SATISFACTORY };

			const oTypeInput = this._createPickerInput({
				placeholder: I18n.getText("typeInputPlaceholder"),
				label: oConfig.typeLabel,
				items: aTypes,
				fields: oConfig.typeFields,
				onPick: (sCode) => { oTypeSelection.code = sCode; },
				selectedCodeHolder: oTypeSelection
			});

			const oNoteArea = new TextArea({ width: "100%", rows: 3 });

			const aFormContent = [
				new Label({ text: oConfig.typeLabel, labelFor: oTypeInput }),
				oTypeInput
			];

			let oResultInput = null;
			if (oConfig.resultField && aResults && aResults.length) {
				const oDefaultResult = aResults.find(
					(r) => r[oConfig.resultFields.code] === Constants.RESULT_CODES.SATISFACTORY
				);

				oResultInput = this._createPickerInput({
					value: oDefaultResult ? oDefaultResult[oConfig.resultFields.text] : "",
					label: oConfig.resultLabel,
					items: aResults,
					fields: oConfig.resultFields,
					flat: true,
					onPick: (sCode) => { oResultSelection.code = sCode; },
					selectedCodeHolder: oResultSelection
				});

				aFormContent.push(
					new Label({ text: oConfig.resultLabel, labelFor: oResultInput }),
					oResultInput
				);
			}

			aFormContent.push(
				new Label({ text: oConfig.noteLabel, labelFor: oNoteArea }),
				oNoteArea
			);

			const oForm = new SimpleForm({
				editable: true,
				layout: "ResponsiveLayout",
				content: aFormContent
			});

			const oDialog = new Dialog({
				title: oConfig.dialogTitle,
				contentWidth: "24rem",
				content: [oForm],
				beginButton: new Button({
					text: I18n.getText("addLabel"),
					type: "Emphasized",
					press: () => {
						if (!oTypeSelection.code) {
							MessageToast.show(I18n.getText("selectTypeMsg", [oConfig.typeLabel.toLowerCase()]));
							return;
						}
						const oProps = {};
						oProps[oConfig.typeField] = oTypeSelection.code;
						if (oConfig.resultField && oResultInput) {
							oProps[oConfig.resultField] = oResultSelection.code;
						}
						oProps[oConfig.noteField] = oNoteArea.getValue();
						oDialog.close();
						this._createRow(oView, oSmartTable, oConfig, oProps);
					}
				}),
				endButton: new Button({
					text: I18n.getText("cancelLabel"),
					press: () => oDialog.close()
				}),
				afterClose: () => oDialog.destroy()
			}).addStyleClass(CSS_CLASSES.ADD_ROW_DIALOG);

			oDialog.open();
		}

		_createRow(oView, oSmartTable, oConfig, oProps) {
			const oHeaderContext = oView.getBindingContext();
			if (ODataContextUtils.isTransient(oHeaderContext)) {
				MessageBox.warning(I18n.getText("saveHeaderFirstMsg"));
				return;
			}
			this._doCreateRow(oView, oHeaderContext, oSmartTable, oConfig, oProps);
		}

		// Единственный источник истины "идёт ли сейчас запрос" — сам промис.
		// _bKpiRefreshPending — debounce-во-время-полёта: если refresh запросили,
		// пока предыдущий выполняется, не плодим параллельный запрос, а перезапускаем один раз после.
		_refreshHeaderKpi(oModel, oHeaderContext) {
			if (this._pKpiRefresh) {
				this._bKpiRefreshPending = true;
				return this._pKpiRefresh;
			}
			this._pKpiRefresh = this._readHeaderKpi(oModel, oHeaderContext)
				.catch(() => MessageToast.show(I18n.getText("kpiRefreshFailedMsg")))
				.then(() => {
					this._pKpiRefresh = null;
					if (this._bKpiRefreshPending) {
						this._bKpiRefreshPending = false;
						this._refreshHeaderKpi(oModel, oHeaderContext);
					}
				});
			return this._pKpiRefresh;
		}

		_readHeaderKpi(oModel, oHeaderContext) {
			const sSelect = [
				F.CHECKS_SUCCESS, F.CHECKS_AMOUNT, F.CHECKS_CRITICALITY, F.HAS_ERROR_CHECKS,
				F.BARRIERS_SUCCESS, F.BARRIERS_AMOUNT, F.BARRIERS_CRITICALITY, F.HAS_ERROR_BARRIERS,
				F.HEADER_KPI_TITLE, F.HEADER_KPI_SUBTITLE
			].join(",");
			return new Promise((resolve, reject) => {
				oModel.read(oHeaderContext.getPath(), {
					urlParameters: { "$select": sSelect },
					success: () => {
						oModel.checkUpdate(true);
						resolve();
					},
					error: reject
				});
			});
		}

		_doCreateRow(oView, oHeaderContext, oSmartTable, oConfig, oProps) {
			const oModel = oView.getModel();
			const oRowContext = oModel.createEntry(oConfig.navProperty, {
				context: oHeaderContext,
				properties: oProps
			});
			oModel.submitChanges({
				success: () => {
					MessageToast.show(I18n.getText("rowAddedMsg"));
					oSmartTable.rebindTable();
					this._refreshHeaderKpi(oModel, oHeaderContext);
				},
				error: () => {
					MessageBox.error(I18n.getText("rowSaveFailedMsg"));
					oModel.deleteCreatedEntry(oRowContext);
				}
			});
		}

		_onDeleteRow(oView, oSmartTable) {
			const oTable = oSmartTable.getTable();
			const aContexts = (oTable && oTable.getSelectedContexts) ? oTable.getSelectedContexts() : [];
			if (!aContexts.length) {
				MessageToast.show(I18n.getText("selectRowToDeleteMsg"));
				return;
			}
			const oModel = oView.getModel();
			MessageBox.confirm(I18n.getText("deleteConfirmMsg", [aContexts.length]), {
				onClose: (sAction) => {
					if (sAction !== MessageBox.Action.OK) {
						return;
					}
					let bHasDeferred = false;
					aContexts.forEach((oContext) => {
						if (ODataContextUtils.isTransient(oContext)) {
							oModel.deleteCreatedEntry(oContext);
						} else {
							oModel.remove(oContext.getPath());
							bHasDeferred = true;
						}
					});
					const oHeaderContext = oView.getBindingContext();
					const fnRebind = () => {
						oSmartTable.rebindTable();
						if (oHeaderContext) {
							this._refreshHeaderKpi(oModel, oHeaderContext);
						}
					};
					if (bHasDeferred) {
						oModel.submitChanges({
							success: fnRebind,
							error: () => {
								MessageBox.error(I18n.getText("deleteFailedMsg"));
								fnRebind();
							}
						});
					} else {
						fnRebind();
					}
				}
			});
		}

		static _forceBlockPopin(oInnerTable) {
			if (!oInnerTable || !oInnerTable.getColumns) {
				return;
			}
			oInnerTable.getColumns().forEach((oColumn) => {
				oColumn.setDemandPopin(true);
				oColumn.setMinScreenWidth(Constants.RESPONSIVE.POPIN_MIN_SCREEN_WIDTH);
				oColumn.setPopinDisplay("Block");
			});
		}
	}

	return SubTableCrud;
});
