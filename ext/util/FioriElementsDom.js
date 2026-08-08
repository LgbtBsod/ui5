sap.ui.define(["./Constants"], function (Constants) {
	"use strict";

	function findElement(sType, fnPredicate, oScope) {
		const fnMatches = (oElement) => oElement.isA && oElement.isA(sType) && fnPredicate(oElement);
		if (oScope && oScope.findAggregatedObjects) {
			return oScope.findAggregatedObjects(true, fnMatches)[0] || null;
		}
		return sap.ui.core.Element.registry.filter(fnMatches)[0] || null;
	}

	function findButtonBySuffix(sSuffix, oScope) {
		const reSuffix = new RegExp(`${sSuffix}$`);
		return findElement("sap.m.Button", (oElement) => reSuffix.test(oElement.getId()), oScope);
	}

	/** Icon fallback for when the auto-generated ID suffix doesn't match. */
	const BUTTON_ICON_MAP = {
		save: "sap-icon://save",
		edit: "sap-icon://edit",
		cancel: "sap-icon://cancel",
		closeColumn: "sap-icon://decline"
	};

	const _oWarnedOnce = new Set();

	/**
	 * Both lookup strategies (ID suffix, icon) are undocumented internal
	 * conventions of sap.suite.ui.generic.template that can change across UI5
	 * patches. Logs once per suffix if both fail, so a framework-side regression
	 * is visible in the console instead of silently doing nothing.
	 */
	function warnLookupFailedOnce(sSuffix) {
		if (_oWarnedOnce.has(sSuffix)) {
			return;
		}
		_oWarnedOnce.add(sSuffix);
		if (window.console && console.warn) {
			console.warn(`FioriElementsDom: button with suffix "${sSuffix}" not found by ID or icon.`);
		}
	}

	function findButtonWithIconFallback(sSuffix, sIconKey, oScope) {
		const oButton = findButtonBySuffix(sSuffix, oScope);
		if (oButton) {
			return oButton;
		}

		const sIcon = BUTTON_ICON_MAP[sIconKey];
		if (!sIcon) {
			warnLookupFailedOnce(sSuffix);
			return null;
		}

		// closeColumn lives in the header, not in the footer-toolbar scope.
		const oScopeForIcon = sIconKey === "closeColumn" ? null : oScope;
		const oFoundByIcon = findElement("sap.m.Button", (oElement) => {
			const oIconAttr = oElement.getIcon ? oElement.getIcon() : null;
			return oIconAttr === sIcon;
		}, oScopeForIcon);

		if (!oFoundByIcon) {
			warnLookupFailedOnce(sSuffix);
		}
		return oFoundByIcon;
	}

	class FioriElementsDom {
		static findElement(sType, fnPredicate, oScope) {
			return findElement(sType, fnPredicate, oScope);
		}

		static findSaveButton(oScope) {
			return findButtonWithIconFallback(Constants.ID_SUFFIXES.SAVE_BUTTON, "save", oScope);
		}

		static findEditButton(oScope) {
			return findButtonWithIconFallback(Constants.ID_SUFFIXES.EDIT_BUTTON, "edit", oScope);
		}

		static findCancelButton(oScope) {
			return findButtonWithIconFallback(Constants.ID_SUFFIXES.CANCEL_BUTTON, "cancel", oScope);
		}

		static findCloseColumnButton(oScope) {
			return findButtonWithIconFallback(Constants.ID_SUFFIXES.CLOSE_COLUMN_BUTTON, "closeColumn", oScope);
		}

		static findNavBackButton(oScope) {
			return findElement("sap.m.Button", (oElement) => oElement.getIcon && oElement.getIcon() === "sap-icon://nav-back", oScope);
		}

		static findFlexibleColumnLayout() {
			return findElement("sap.f.FlexibleColumnLayout", () => true);
		}

		/**
		 * Finds a button by ID suffix in a pre-built snapshot (one tree walk
		 * shared across lookups) if given, otherwise falls back to a full scan.
		 */
		static findButtonInSnapshotOrScope(sSuffix, aSnapshot, oScope) {
			const reSuffix = new RegExp(`${sSuffix}$`);
			if (aSnapshot) {
				return aSnapshot.find((oElement) => reSuffix.test(oElement.getId())) || null;
			}
			return findElement("sap.m.Button", (oElement) => reSuffix.test(oElement.getId()), oScope);
		}

		/** sap.ui.comp.smarttable.SmartTable exposes its toolbar via different getters depending on config. */
		static getSmartTableToolbar(oSmartTable) {
			return oSmartTable.getCustomToolbar ? oSmartTable.getCustomToolbar()
				: (oSmartTable.getToolbar && oSmartTable.getToolbar());
		}
	}

	return FioriElementsDom;
});
