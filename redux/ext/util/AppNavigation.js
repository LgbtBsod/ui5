sap.ui.define([], function () {
	"use strict";

	/**
	 * Single "return to the root list" implementation, shared by
	 * NotFoundRecovery.js and DiscardTransientOnClose.js.
	 */
	class AppNavigation {
		/** @returns {sap.ui.core.Component|null} the app's root Component, if any is registered. */
		static findAppComponent() {
			const oRegistry = sap.ui.core.Component.registry;
			if (!oRegistry || typeof oRegistry.all !== "function") {
				return null;
			}
			return Object.values(oRegistry.all()).find((oComp) => {
				try {
					return !!(oComp && oComp.getManifest
						&& oComp.getManifest()["sap.app"]
						&& oComp.getManifest()["sap.app"].id);
				} catch (e) {
					return false;
				}
			}) || null;
		}

		/**
		 * Router.navTo (preferred) preserves app state (unsaved-changes warning,
		 * OData model cache, FCL state). HashChanger is a real fallback for calls
		 * that race component/router bootstrap. location.href is the last resort.
		 */
		static navigateToRoot() {
			try {
				const oComponent = AppNavigation.findAppComponent();
				if (oComponent) {
					const oRouter = sap.ui.core.UIComponent.getRouterFor(oComponent);
					if (oRouter && typeof oRouter.navTo === "function") {
						oRouter.navTo("");
						return;
					}
				}
				const oHashChanger = sap.ui.core.routing.HashChanger.getInstance();
				if (oHashChanger) {
					oHashChanger.replaceHash("");
					return;
				}
			} catch (e) {
				// fall through to location.href
			}
			window.location.href = window.location.origin + window.location.pathname;
		}
	}

	return AppNavigation;
});
