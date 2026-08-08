sap.ui.define(["./FioriElementsDom", "./WatchedButton"], function (FioriElementsDom, WatchedButton) {
	"use strict";

	/**
	 * Single source of "was Save (not Cancel) the last footer action pressed".
	 * KeepEditMode and DiscardTransientOnClose both need this signal to tell
	 * apart a Save-triggered editable->false transition from a Cancel-triggered
	 * one; previously each wired its own Save/Cancel WatchedButton pair and
	 * tracked the flag independently.
	 */
	class SaveIntentTracker {
		constructor() {
			this._oSaveButton = new WatchedButton(FioriElementsDom.findSaveButton);
			this._oCancelButton = new WatchedButton(FioriElementsDom.findCancelButton);
			this._bSaveIntent = false;
		}

		/** @param {sap.ui.core.mvc.View} oView */
		refresh(oView) {
			this._oSaveButton.watchOnce(oView, () => { this._bSaveIntent = true; });
			this._oCancelButton.watchOnce(oView, () => { this._bSaveIntent = false; });
		}

		/** @returns {boolean} true if Save (not Cancel) was the last footer action pressed. */
		wasSaveIntended() {
			return this._bSaveIntent;
		}

		/** Resets the flag after a consumer has acted on an editable->false transition. */
		reset() {
			this._bSaveIntent = false;
		}
	}

	return SaveIntentTracker;
});
