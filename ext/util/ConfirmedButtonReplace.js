sap.ui.define(["./OnceRegistry"], function (OnceRegistry) {
	"use strict";

	/**
	 * Swaps a standard Fiori Elements footer button for a custom one, once.
	 *
	 * Use this only for actions that must ALWAYS behave differently than the
	 * framework default (e.g. Delete always needs confirmation) — the swap is
	 * permanent and the original control is destroyed. For actions whose
	 * required behavior depends on runtime state (e.g. Cancel differs for a
	 * transient vs. a saved record), react to state instead — see
	 * EditableTransitionGate / SaveIntentTracker.
	 */
	class ConfirmedButtonReplace {
		constructor() {
			this._oReplaced = new OnceRegistry();
		}

		/**
		 * @param {sap.m.Button|null} oOriginalButton found via a FioriElementsDom lookup.
		 * @param {function(sap.m.Button): sap.m.Button} fnBuildReplacement builds the replacement button.
		 * @returns {sap.m.Button|null} the replacement, or null if already replaced / no valid parent.
		 */
		replaceOnce(oOriginalButton, fnBuildReplacement) {
			if (!oOriginalButton || this._oReplaced.isMarked(oOriginalButton)) {
				return null;
			}
			const oParent = oOriginalButton.getParent();
			if (!oParent || typeof oParent.indexOfContent !== "function") {
				return null;
			}
			const iIndex = oParent.indexOfContent(oOriginalButton);
			const oReplacement = fnBuildReplacement(oOriginalButton);
			oParent.removeContent(oOriginalButton);
			oOriginalButton.destroy();
			oParent.insertContent(oReplacement, iIndex);
			this._oReplaced.mark(oReplacement);
			return oReplacement;
		}

		destroy() {
			this._oReplaced = null;
		}
	}

	return ConfirmedButtonReplace;
});
