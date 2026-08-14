sap.ui.define(["./Constants", "./FioriElementsDom", "./WatchedButton", "./CachedElementLookup", "./I18n", "sap/m/MessageBox"], function (Constants, FioriElementsDom, WatchedButton, CachedElementLookup, I18n, MessageBox) {
	"use strict";

	const F = Constants.FIELDS;
	const WARNING_TEXT = I18n.getText("integrationEditWarningMsg");

	// [Fix, use-case pass #4] Integration-sourced records are view-only end to
	// end now - per the real business rule, there was never a valid "continue
	// editing anyway" choice here (re-syncing from the integration feed
	// wouldn't pick up local edits either, see integrationEditWarningMsg), so
	// the old MessageBox.confirm's OK-to-proceed path was always a false
	// affordance. Primary block is annotation-driven (sap:updatable-path=
	// "Updatable" on CheckRoots, metadata.xml/check-root.xml -> disables the
	// "Редактировать" button itself, see resolvers.compute_check_root_view)
	// - this guard is the message layered on top / backstop if that button
	// somehow still fires press. Plain warning (OK-only), unconditionally
	// cancels edit mode - no confirmed/remembered-consent state anymore.
	class IntegrationEditGuard {
		constructor() {
			this._oEditButton = new WatchedButton(FioriElementsDom.findEditButton);
			this._oCancelButtonLookup = new CachedElementLookup(FioriElementsDom.findCancelButton);
		}

		refresh(oView) {
			this._oEditButton.watchOnce(oView, () => this._onEditPressed(oView));
		}

		_onEditPressed(oView) {
			const oContext = oView.getBindingContext();
			if (!oContext || !oContext.getProperty(F.THIS_IS_INTEGRATION_DATA)) {
				return;
			}
			MessageBox.warning(WARNING_TEXT, {
				onClose: () => this._cancelEdit(oView)
			});
		}

		_cancelEdit(oView) {
			const oCancelButton = this._oCancelButtonLookup.get(oView);
			if (oCancelButton) {
				oCancelButton.firePress();
			}
		}
	}

	return IntegrationEditGuard;
});
