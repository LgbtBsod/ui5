sap.ui.define([
	"./FioriElementsDom", "./WatchedButton", "./ODataContextUtils", "./AppNavigation"
], function (FioriElementsDom, WatchedButton, ODataContextUtils, AppNavigation) {
	"use strict";

	/**
	 * Cleans up a transient (never-saved) create when the user backs out of it —
	 * via the column-close (X) button, or via Cancel.
	 *
	 * The framework does not call `deleteCreatedEntry()` for either action on a
	 * transient object, leaving it pending in the model.
	 *
	 * Correctness note: this must NOT fire on Cancel of an already-saved record
	 * (that should just exit edit mode, handled entirely by the framework).
	 * Distinguishing the two only from a button click is unreliable, since a
	 * transient object's own Save also flips `ui>/editable` false. Instead this
	 * watches the shared editable-transition signal (oSnapshot, computed once by
	 * ObjectPageExtension via EditableTransitionGate/SaveIntentTracker) and only
	 * acts when the transition happened while the context was transient AND
	 * Save was not the action that caused it.
	 *
	 * Uses only standard control APIs (WatchedButton -> attachPress) — no DOM
	 * event interception, no button replacement.
	 */
	class DiscardTransientOnClose {
		constructor() {
			this._oCloseButton = new WatchedButton(FioriElementsDom.findCloseColumnButton);
			this._oPendingTransientContext = null;
		}

		/**
		 * @param {sap.ui.core.mvc.View} oView
		 * @param {{editableFell: boolean, saveWasIntended: boolean}} oSnapshot
		 */
		refresh(oView, oSnapshot) {
			this._oCloseButton.watchOnce(oView, () => this._onClosePressed(oView));

			const oContext = oView.getBindingContext();
			if (oContext && ODataContextUtils.isUnsavedNewDraft(oContext)) {
				this._oPendingTransientContext = oContext;
			}

			if (oSnapshot.editableFell && !oSnapshot.saveWasIntended && this._oPendingTransientContext) {
				DiscardTransientOnClose._discardAndReturn(oView, this._oPendingTransientContext);
			}
			if (!oContext || !ODataContextUtils.isUnsavedNewDraft(oContext)) {
				this._oPendingTransientContext = null;
			}
		}

		_onClosePressed(oView) {
			const oContext = oView.getBindingContext();
			if (oContext && ODataContextUtils.isUnsavedNewDraft(oContext)) {
				DiscardTransientOnClose._discardAndReturn(oView, oContext);
			}
		}

		destroy() {
			this._oPendingTransientContext = null;
		}

		/**
		 * [Fix, minimal-extension-set pass] Was `oModel.deleteCreatedEntry(oContext)`
		 * - the sap.ui.model.odata.v2.ODataModel API for discarding a
		 * client-side-only entry made via `createEntry()`. This app's real
		 * create-drafts are already-POSTed server-side entities (see
		 * ODataContextUtils.isUnsavedNewDraft's comment), so
		 * deleteCreatedEntry() found nothing to discard and silently no-opped
		 * (swallowed by the try/catch below, which never even fired) - the
		 * abandoned draft row stayed on the server forever. The real,
		 * already-proven discard mechanism for this app is a plain HTTP
		 * DELETE on the draft entity (see annotations.xml's Common.DraftRoot
		 * comment: "Discard in the real convention is a plain HTTP DELETE on
		 * the draft entity, which serve.py's DELETE handler already
		 * implements identically") - `oModel.remove()` is the v2 ODataModel
		 * call for exactly that. Confirmed live: the draft row 404s
		 * immediately after.
		 */
		static _discardAndReturn(oView, oContext) {
			try {
				oView.getModel().remove(oContext.getPath());
			} catch (e) {
				// context already gone
			}
			DiscardTransientOnClose._returnToListReport();
		}

		/** Resets FlexibleColumnLayout to one column before navigating back. */
		static _returnToListReport() {
			try {
				const oFcl = FioriElementsDom.findFlexibleColumnLayout();
				if (oFcl && typeof oFcl.setLayout === "function") {
					oFcl.setLayout("OneColumn");
				}
			} catch (e) {
				// no FCL in current layout
			}
			AppNavigation.navigateToRoot();
		}
	}

	return DiscardTransientOnClose;
});
