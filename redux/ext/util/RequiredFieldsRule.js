sap.ui.define(["./I18n"], function (I18n) {
	"use strict";

	// Classic (non-draft) Fiori Elements Smart Template на OData v2 не
	// валидирует required-поля перед Save — нужен явный гейт (см. FieldStateGate).
	// Список полей читается из ODataMetaModel (Common.Required/Common.Label),
	// а не дублируется вручную — annotations.xml остаётся единственным
	// источником истины для того, какие поля обязательны.
	const REQUIRED_ANNOTATION = "com.sap.vocabularies.Common.v1.Required";
	const LABEL_ANNOTATION = "com.sap.vocabularies.Common.v1.Label";
	const ENTITY_TYPE = "ZCheckService.CheckRoot";

	// Кэш ключуется по экземпляру модели (WeakMap), а не разделяется между
	// моделями — при смене сервиса/метаданных не должен отдавать список
	// required-полей от старой модели.
	const _oCachedFieldsByModel = new WeakMap();

	function isEmpty(vValue) {
		return vValue === undefined || vValue === null || vValue === ""
			|| (typeof vValue === "object" && vValue.ms === undefined && !(vValue instanceof Date));
	}

	// Метаданные неизменны в рамках сессии одной модели — резолвим список
	// обязательных полей один раз на модель и кэшируем, а не на каждый вызов getIssues.
	function getRequiredFields(oModel) {
		if (_oCachedFieldsByModel.has(oModel)) {
			return _oCachedFieldsByModel.get(oModel);
		}
		const oMetaModel = oModel.getMetaModel();
		const oEntityType = oMetaModel.getODataEntityType(ENTITY_TYPE);
		if (!oEntityType) {
			return [];
		}
		const aFields = (oEntityType.property || [])
			.filter((oProp) => oProp[REQUIRED_ANNOTATION] && oProp[REQUIRED_ANNOTATION].Bool === "true")
			.map((oProp) => ({
				field: oProp.name,
				label: (oProp[LABEL_ANNOTATION] && oProp[LABEL_ANNOTATION].String) || oProp.name
			}));
		_oCachedFieldsByModel.set(oModel, aFields);
		return aFields;
	}

	class RequiredFieldsRule {
		static getIssues(oView) {
			const oContext = oView.getBindingContext();
			if (!oContext) {
				return [];
			}
			const oModel = oView.getModel();
			return getRequiredFields(oModel)
				.filter((oRule) => isEmpty(oContext.getProperty(oRule.field)))
				.map((oRule) => RequiredFieldsRule._toIssue(oContext, oRule));
		}

		static _toIssue(oContext, oRule) {
			return {
				target: `${oContext.getPath()}/${oRule.field}`,
				message: I18n.getText("requiredFieldMsg", [oRule.label])
			};
		}
	}

	return RequiredFieldsRule;
});
