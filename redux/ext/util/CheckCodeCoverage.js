sap.ui.define(["./Constants", "./FioriElementsDom", "./CachedElementLookup", "./I18n"],
	function (Constants, FioriElementsDom, CachedElementLookup, I18n) {
	"use strict";

	const F = Constants.FIELDS;

	// getIssues() вызывается на каждый тик валидации, поэтому поиск
	// SmartTable кэшируется, а не повторяет обход дерева view.
	class CheckCodeCoverage {
		constructor() {
			this._aIssues = [];
			this._oWatchedBinding = null;
			this._oSmartTableLookup = new CachedElementLookup((oView) =>
				FioriElementsDom.findElement(
					"sap.ui.comp.smarttable.SmartTable",
					(oElement) => oElement.getId().indexOf(Constants.FACET_IDS.CHECK_ROWS_SECTION) !== -1,
					oView
				)
			);
		}

		getIssues(oView) {
			this._syncWatch(oView);
			return this._aIssues;
		}

		_syncWatch(oView) {
			const oSmartTable = this._oSmartTableLookup.get(oView);
			const oInnerTable = oSmartTable && oSmartTable.getTable && oSmartTable.getTable();
			const oBinding = oInnerTable && oInnerTable.getBinding && oInnerTable.getBinding("items");
			if (!oBinding || oBinding === this._oWatchedBinding) {
				return;
			}
			this._oWatchedBinding = oBinding;
			oBinding.attachChange(() => this._recompute());
			this._recompute();
		}

		_recompute() {
			const aContexts = this._oWatchedBinding.getContexts();
			this._aIssues = aContexts
				.filter((oRowContext) => {
					const sCode = oRowContext.getProperty(F.CODE);
					if (!sCode) {
						return true;
					}
					return sCode === Constants.SENTINELS.CHECK_TYPE_RAW_TEXT_CODE && !oRowContext.getProperty(F.RAW_TEXT);
				})
				.map((oRowContext) => ({
					target: `${oRowContext.getPath()}/${F.TEXT}`,
					message: CheckCodeCoverage.TOOLTIP
				}));
		}
	}

	CheckCodeCoverage.TOOLTIP = I18n.getText("checkCodeCoverageTooltip");

	return CheckCodeCoverage;
});
