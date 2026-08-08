sap.ui.define(["./Constants", "sap/ui/core/Item"], function (Constants, Item) {
	"use strict";

	// Suggest для полей со справочником, уже загруженным целиком в память
	// (CheckTypes/BarrierTypes/CheckResults) — без нового HTTP на ввод.
	class LocalItemSuggest {
		/**
		 * @param {sap.m.Input} oInput
		 * @param {object[]} aItems
		 * @param {object} oFields - { code, text }
		 */
		static wire(oInput, aItems, oFields) {
			oInput.setShowSuggestion(true);
			oInput.setStartSuggestion(Constants.SUGGEST.MIN_CHARS);
			oInput.setFilterSuggests(true);
			if (oInput.setAutocomplete) {
				oInput.setAutocomplete(false);
			}
			aItems.forEach((oItem) => {
				oInput.addSuggestionItem(new Item({ key: oItem[oFields.code], text: oItem[oFields.text] }));
			});
		}
	}

	return LocalItemSuggest;
});
