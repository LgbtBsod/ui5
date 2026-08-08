sap.ui.define([
	"./FioriElementsDom",
	"./AppNavigation",
	"./I18n",
	"sap/ui/core/BusyIndicator",
	"sap/m/FlexBox",
	"sap/m/Button",
	"sap/m/Text"
], function (FioriElementsDom, AppNavigation, I18n, BusyIndicator, FlexBox, Button, Text) {
	"use strict";

	const LITE_PARAM = "lite";
	const LITE_ROUTE = "createLite"; // see manifest.json sap.ui5.routing.routes
	const ACTIVE_CLASS = "pcLiteActive";
	const HEIGHT_VAR = "--pc-lite-banner-height";

	/**
	 * Deep-links `?lite` straight into the Object Page create screen via the
	 * `createLite` route (manifest.json), instead of polling for the ListReport
	 * "Create" button and simulating a click on it.
	 */
	class LiteCreateMode {
		constructor() {
			this._oBanner = null;
		}

		start() {
			if (!LiteCreateMode._isActive()) {
				return;
			}
			BusyIndicator.show(0);

			const oComponent = AppNavigation.findAppComponent();
			const oRouter = oComponent && sap.ui.core.UIComponent.getRouterFor(oComponent);
			const oRoute = oRouter && oRouter.getRoute && oRouter.getRoute(LITE_ROUTE);
			if (!oRoute) {
				BusyIndicator.hide();
				return;
			}

			oRoute.attachPatternMatched(() => {
				BusyIndicator.hide();
				this._showBanner();
			}, this, true);
			oRouter.navTo(LITE_ROUTE);
		}

		stop() {
			this._destroyBanner();
			BusyIndicator.hide();
		}

		_showBanner() {
			LiteCreateMode._hideBackButton();
			if (this._oBanner) {
				return;
			}
			this._oBanner = new FlexBox({
				direction: "Column",
				alignItems: "Center",
				items: [
					new Text({ text: I18n.getText("liteBannerText"), wrapping: true, textAlign: "Center" }),
					new FlexBox({
						direction: "Row",
						justifyContent: "Center",
						alignItems: "Center",
						items: [
							new Button({
								text: I18n.getText("liteBannerButtonText"),
								type: "Emphasized",
								press: () => LiteCreateMode._reloadFresh()
							}),
							new Button({
								icon: "sap-icon://decline",
								type: "Transparent",
								tooltip: I18n.getText("liteBannerHideTooltip"),
								press: () => this._destroyBanner()
							})
						]
					}).addStyleClass("sapUiTinyMarginTop")
				]
			}).addStyleClass("pcLiteBanner sapUiTinyMargin sapUiSizeCompact");
			this._oBanner.addEventDelegate({
				onAfterRendering: () => LiteCreateMode._syncBannerHeight(this._oBanner)
			});
			document.documentElement.classList.add(ACTIVE_CLASS);
			this._oBanner.placeAt(document.body, "first");
		}

		/** Explicit destroy prevents leaking the banner's control tree across repeated start()/stop(). */
		_destroyBanner() {
			if (this._oBanner) {
				this._oBanner.destroy();
				this._oBanner = null;
			}
			document.documentElement.classList.remove(ACTIVE_CLASS);
			document.documentElement.style.removeProperty(HEIGHT_VAR);
		}

		static _syncBannerHeight(oBanner) {
			const oDomRef = oBanner.getDomRef();
			if (!oDomRef) {
				return;
			}
			document.documentElement.style.setProperty(HEIGHT_VAR, `${oDomRef.offsetHeight}px`);
			// Force ObjectPageLayout to recompute its floating-footer offset now that
			// the banner exists.
			window.dispatchEvent(new Event("resize"));
		}

		static _isActive() {
			return new URLSearchParams(window.location.search).has(LITE_PARAM);
		}

		static _hideBackButton() {
			const oBackButton = FioriElementsDom.findNavBackButton();
			if (oBackButton) {
				oBackButton.setVisible(false);
			}
		}

		static _reloadFresh() {
			window.location.href = window.location.origin + window.location.pathname + window.location.search;
		}
	}

	return LiteCreateMode;
});
