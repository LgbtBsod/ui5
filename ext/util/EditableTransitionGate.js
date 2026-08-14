sap.ui.define(["./Constants"], function (Constants) {
	"use strict";

	/**
	 * Detects the exact tick where `ui>/editable` flips from `true` to `false`.
	 * Originally extracted from KeepEditMode (removed in the legacy-guard
	 * audit pass, see ObjectPageExtension.controller.js), which duplicated
	 * this bookkeeping; now the single source of "did edit mode just end" for
	 * DiscardTransientOnClose, the remaining consumer.
	 */
	class EditableTransitionGate {
		constructor() {
			this._bWasEditable = false;
		}

		/** @returns {boolean} whether `ui>/editable` is currently true for oView. */
		static isEditable(oView) {
			const oUiModel = oView.getModel(Constants.UI_MODEL.NAME);
			return !!(oUiModel && oUiModel.getProperty(Constants.UI_MODEL.EDITABLE_PATH));
		}

		/**
		 * @param {sap.ui.core.mvc.View} oView
		 * @returns {boolean} true exactly once, on the tick where editable fell true->false.
		 */
		consumeFalling(oView) {
			const bEditable = EditableTransitionGate.isEditable(oView);
			const bFell = this._bWasEditable && !bEditable;
			this._bWasEditable = bEditable;
			return bFell;
		}
	}

	return EditableTransitionGate;
});
