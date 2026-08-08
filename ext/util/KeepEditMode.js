sap.ui.define(["./FioriElementsDom", "./CachedElementLookup"], function (FioriElementsDom, CachedElementLookup) {
	"use strict";

	/**
	 * Re-enters edit mode right after the first Save of a transient object.
	 *
	 * The framework rebuilds the footer toolbar on first save, so a press
	 * listener attached to the old Save button never fires — the transition
	 * of `ui>/editable` (true->false) is watched instead, via the shared
	 * {@link EditableTransitionGate}/{@link SaveIntentTracker}, computed once
	 * per tick by ObjectPageExtension and passed in as oSnapshot fields.
	 */
	class KeepEditMode {
		constructor() {
			this._oEditButtonLookup = new CachedElementLookup(FioriElementsDom.findEditButton);
		}

		/**
		 * @param {sap.ui.core.mvc.View} oView
		 * @param {{editableFell: boolean, saveWasIntended: boolean}} oSnapshot
		 */
		refresh(oView, oSnapshot) {
			if (!oSnapshot.editableFell || !oSnapshot.saveWasIntended) {
				return;
			}
			const oEditButton = this._oEditButtonLookup.get(oView);
			if (oEditButton) {
				oEditButton.firePress();
			}
		}
	}

	return KeepEditMode;
});
