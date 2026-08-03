sap.ui.define([
	"sap/m/MessageBox",
	"sap/m/Button",
	"./FioriElementsDom",
	"./ConfirmedButtonReplace",
	"./I18n",
	"./Constants"
], function (MessageBox, Button, FioriElementsDom, ConfirmedButtonReplace, I18n, Constants) {
	"use strict";

	/**
	 * Requires confirmation before deleting an Object Page record.
	 *
	 * The classic (non-draft) generic template deletes on a single click with
	 * no confirmation. The standard Delete button is swapped once for a custom
	 * one (see ConfirmedButtonReplace) whose press shows a MessageBox and, on
	 * confirmation, calls `oModel.remove()` directly — this replaces the
	 * framework's own delete handling entirely and unconditionally, which is
	 * correct here since deletion should always be confirmed.
	 */
	class DeleteConfirmGuard {
		constructor() {
			this._oReplace = new ConfirmedButtonReplace();
		}

		/**
		 * @param {sap.ui.core.mvc.View} oView
		 * @param {sap.m.Button[]} [aButtons] snapshot from ObjectPageExtension._buildScanSnapshot.
		 */
		refresh(oView, aButtons) {
			const oOriginal = FioriElementsDom.findButtonInSnapshotOrScope(
				Constants.ID_SUFFIXES.DELETE_ENTRY_BUTTON, aButtons, oView
			);
			this._oReplace.replaceOnce(oOriginal, (oOrig) => new Button({
				text: oOrig.getText(),
				icon: oOrig.getIcon(),
				type: oOrig.getType(),
				enabled: oOrig.getEnabled(),
				press: () => this._onPress(oView)
			}));
		}

		_onPress(oView) {
			const oContext = oView.getBindingContext();
			if (!oContext) {
				return;
			}
			MessageBox.confirm(I18n.getText("deleteConfirmDialogMsg"), {
				title: I18n.getText("deleteConfirmTitle"),
				icon: MessageBox.Icon.QUESTION,
				actions: [MessageBox.Action.DELETE, MessageBox.Action.CANCEL],
				emphasizedAction: MessageBox.Action.DELETE,
				onClose: (sAction) => {
					if (sAction === MessageBox.Action.DELETE) {
						oView.getModel().remove(oContext.getPath());
					}
				}
			});
		}

		destroy() {
			if (this._oReplace) {
				this._oReplace.destroy();
				this._oReplace = null;
			}
		}
	}

	return DeleteConfirmGuard;
});
