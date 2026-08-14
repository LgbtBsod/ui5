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
			// [Fix, use-case pass #4] Integration-sourced records are view-only
			// now (see IntegrationEditGuard.js / metadata.xml sap:updatable-path)
			// - this rule exists to prompt the inspector to pick a proper
			// CheckType via F4 instead of leaving a raw-text row uncategorized;
			// on a record nobody can edit anymore there's no F4 to open, so the
			// tooltip would just be a dangling error. RawText still displays
			// correctly regardless (resolvers.py's raw_text_fallback) - this
			// only needs to gate rows on records someone can actually edit.
			const oHeaderContext = oView.getBindingContext();
			if (oHeaderContext && oHeaderContext.getProperty(F.THIS_IS_INTEGRATION_DATA)) {
				return [];
			}
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
