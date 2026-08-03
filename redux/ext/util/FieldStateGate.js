sap.ui.define([
	"./Constants", "./FioriElementsDom", "./CachedElementLookup", "./I18n",
	"sap/ui/core/message/Message", "sap/ui/core/MessageType", "sap/ui/core/Core"
], function (Constants, FioriElementsDom, CachedElementLookup, I18n, Message, MessageType, Core) {
	"use strict";

	const BLOCKED_TOOLTIP = I18n.getText("saveRequiredFieldsTooltip");

	/**
	 * Publishes client-side validation issues as `sap.ui.core.message.Message`,
	 * so `MessageManager` binds them to the target controls' valueState — no
	 * manual control<->path bookkeeping.
	 *
	 * `technical: true` prevents classic Fiori Elements v1 from auto-opening the
	 * "Messages" popup (which only reacts to `validation: true`), without
	 * bypassing the message infrastructure entirely.
	 */
	class FieldStateGate {
		/**
		 * @param {string} sName gate identifier (used only for readability at call sites).
		 * @param {function(sap.ui.core.mvc.View): {target: string, message: string}[]} fnGetIssues
		 * @param {{blocksSave?: boolean}} [oOptions]
		 */
		constructor(sName, fnGetIssues, oOptions) {
			this._sName = sName;
			this._fnGetIssues = fnGetIssues;
			this._bBlocksSave = !!(oOptions && oOptions.blocksSave);
			this._oMessageManager = Core.getMessageManager();
			this._aOwnMessages = [];
			if (this._bBlocksSave) {
				this._oSaveButtonLookup = new CachedElementLookup(FioriElementsDom.findSaveButton);
			}
		}

		/** @param {sap.ui.core.mvc.View} oView */
		refresh(oView) {
			const oModel = oView.getModel();
			const aIssues = this._fnGetIssues(oView) || [];

			this._oMessageManager.removeMessages(this._aOwnMessages);
			this._aOwnMessages = aIssues.map((oIssue) => new Message({
				message: oIssue.message,
				type: MessageType.Error,
				target: oIssue.target,
				processor: oModel,
				technical: true,
				persistent: false
			}));
			this._oMessageManager.addMessages(this._aOwnMessages);

			if (this._bBlocksSave) {
				const oSaveButton = this._oSaveButtonLookup.get(oView);
				if (oSaveButton) {
					oSaveButton.setEnabled(aIssues.length === 0);
					oSaveButton.setTooltip(aIssues.length ? BLOCKED_TOOLTIP : "");
				}
			}
			return aIssues;
		}

		destroy() {
			this._oMessageManager.removeMessages(this._aOwnMessages);
			this._aOwnMessages = [];
		}
	}

	return FieldStateGate;
});
