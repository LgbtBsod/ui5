sap.ui.define([], function () {
	"use strict";

	const DUPLICATE_PRONE_TYPES = ["sap.ui.layout.form.Form", "sap.ui.comp.smarttable.SmartTable"];

	/**
	 * Works around a SAPUI5 1.71 ObjectPageLayout component-reuse bug: navigating
	 * from an already-open record to "Create" leaves a stale DOM copy of a
	 * section under the same control id (`document.getElementById` then resolves
	 * to the first, outdated, node — affecting Form and SmartTable rendering).
	 * Removes all but one node sharing that id.
	 */
	class StaleDisplayFormGuard {

		/**
		 * @param {sap.ui.core.mvc.View} oView
		 * @param {sap.ui.core.Control[]} [aDuplicateProneControls] snapshot from
		 *   ObjectPageExtension._buildScanSnapshot.
		 */
		refresh(oView, aDuplicateProneControls) {
			const oScopeDom = oView.getDomRef();
			if (!oScopeDom) {
				return;
			}
			(aDuplicateProneControls || StaleDisplayFormGuard._findDuplicateProneControls(oView))
				.forEach((oControl) => StaleDisplayFormGuard._removeDuplicateDom(oControl, oScopeDom));
		}

		static _removeDuplicateDom(oControl, oScopeDom) {
			const sId = oControl.getId();
			const aNodes = oScopeDom.querySelectorAll(`[id="${CSS.escape(sId)}"]`);
			if (aNodes.length <= 1) {
				return;
			}
			if (aNodes.length > 2) {
				// The known bug leaves exactly one stale + one live node. More than
				// that means the framework's behavior no longer matches this
				// assumption — don't guess which node to keep, just flag it.
				if (window.console && console.warn) {
					console.warn(`StaleDisplayFormGuard: ${aNodes.length} nodes found for id="${sId}" (expected 2) — skipping.`);
				}
				return;
			}
			// The live node is usually the last in document order, but if the user
			// is typing into a field under the other one, removing it detaches the
			// active element and breaks input — so the focused node always survives.
			const oActive = document.activeElement;
			const oFocusedNode = oActive && Array.prototype.find.call(aNodes, (oNode) => oNode.contains(oActive));
			const oSurvivor = oFocusedNode || aNodes[aNodes.length - 1];
			Array.prototype.filter.call(aNodes, (oNode) => oNode !== oSurvivor)
				.forEach((oStaleNode) => oStaleNode.remove());
		}

		static _findDuplicateProneControls(oView) {
			return oView.findAggregatedObjects(true,
				(oElement) => oElement.isA && DUPLICATE_PRONE_TYPES.some((sType) => oElement.isA(sType)));
		}
	}

	return StaleDisplayFormGuard;
});
