sap.ui.define([], function () {
	"use strict";

	class ODataContextUtils {
		/**
		 * [Fix, minimal-extension-set pass] Was `isTransient` and checked
		 * `oContext.isTransient()`/`bCreated` - the sap.ui.model.odata.v2.
		 * Context API for an entry created client-side via
		 * `oModel.createEntry()`, which this app never uses. Every
		 * "Создать" here goes through the real draft-based Fiori Elements
		 * flow: navigating to the create route immediately calls
		 * CheckRootPreparationAction server-side and binds the view to a
		 * REAL, already-POSTed draft entity (a real DraftUUID, ActiveUUID =
		 * zero-guid) - so `isTransient()`/`bCreated` was always false, and
		 * every guard gated on it (DateTimeAutofill, KpiSync,
		 * DiscardTransientOnClose) silently never ran on create. Confirmed
		 * live: a fresh "Создать" left Date/Time/Timezone permanently empty,
		 * and closing an abandoned create without Save never called
		 * deleteCreatedEntry server-side - leaking a real draft row forever.
		 *
		 * The correct signal for "this is a brand-new, unsaved draft with no
		 * active counterpart" in this app's real draft architecture is the
		 * standard Common.DraftRoot pair: !IsActiveEntity && !HasActiveEntity
		 * (true only for a fresh create-draft; an edit-draft of an EXISTING
		 * object has HasActiveEntity=true, and the active/display view has
		 * IsActiveEntity=true) - so an already-active record being edited is
		 * correctly excluded, same as the old check intended.
		 *
		 * [Fix, use-case pass] Second bug found live: right at the instant
		 * "Редактировать" starts an edit session on an EXISTING (already-
		 * active) record, `HasActiveEntity` briefly reads as `undefined` on
		 * the very first _scanTick before the freshly-bound draft context's
		 * properties have fully round-tripped - `!undefined` is `true` in
		 * JS, so this momentarily misclassified an EXISTING record's own
		 * edit-draft as a brand-new unsaved one. DiscardTransientOnClose
		 * latches that one bad reading into `_oPendingTransientContext` and
		 * never gets a chance to correct it before the user clicks
		 * "Отменить" - discarding (deleting) a draft of an EXISTING record
		 * instead of just leaving edit mode, then bouncing to the List
		 * Report when the now-vanished context 404s. Confirmed with a
		 * temporary diagnostic log capturing the exact tick. Treating an
		 * unloaded (`undefined`) HasActiveEntity as "unknown, not yet safe
		 * to call this a new draft" - rather than defaulting to `true` via
		 * JS's falsy coercion - closes the race: a real new create's
		 * HasActiveEntity settles to `false` within the same tick or the
		 * next one, so DiscardTransientOnClose still catches it correctly a
		 * moment later, same as it always intended to.
		 */
		static isUnsavedNewDraft(oContext) {
			if (!oContext) {
				return false;
			}
			const bHasActiveEntity = oContext.getProperty("HasActiveEntity");
			if (bHasActiveEntity === undefined) {
				return false;
			}
			return !oContext.getProperty("IsActiveEntity") && !bHasActiveEntity;
		}
	}

	return ODataContextUtils;
});
