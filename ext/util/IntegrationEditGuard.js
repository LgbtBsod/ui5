sap.ui.define(["./Constants", "./FioriElementsDom", "./WatchedButton", "./CachedElementLookup", "./I18n", "sap/m/MessageBox"], function (Constants, FioriElementsDom, WatchedButton, CachedElementLookup, I18n, MessageBox) {
	"use strict";

	const F = Constants.FIELDS;
	const WARNING_TEXT = I18n.getText("integrationEditWarningMsg");

	class IntegrationEditGuard {
		constructor() {
			this._oEditButton = new WatchedButton(FioriElementsDom.findEditButton);
			this._oCancelButtonLookup = new CachedElementLookup(FioriElementsDom.findCancelButton);
			this._sConfirmedPath = null;
		}

		refresh(oView) {
			this._oEditButton.watchOnce(oView, () => this._onEditPressed(oView));
		}

		_onEditPressed(oView) {
			const oContext = oView.getBindingContext();
			if (!oContext || !oContext.getProperty(F.THIS_IS_INTEGRATION_DATA)) {
				return;
			}
			const sPath = oContext.getPath();
			if (this._sConfirmedPath === sPath) {
				return;
			}
			MessageBox.confirm(WARNING_TEXT, {
				onClose: (sAction) => {
					if (sAction === MessageBox.Action.OK) {
						this._sConfirmedPath = sPath;
					} else {
						this._cancelEdit(oView);
					}
				}
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
