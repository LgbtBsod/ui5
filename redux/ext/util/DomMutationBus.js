sap.ui.define([], function () {
	"use strict";

	/**
	 * One shared MutationObserver for the whole app, scoped to SAPUI5's static
	 * UI area (`sap.ui.getCore().getStaticAreaRef()`) — where dialogs and
	 * popups actually render — rather than `document.body`, so form/table
	 * re-renders elsewhere in the page don't reach subscribers that only care
	 * about dialogs (ValueHelpAutoApply, NotFoundRecovery).
	 */
	class DomMutationBus {
		constructor() {
			this._aListeners = [];
			this._oObserver = null;
		}

		/** @param {function(MutationRecord[]): void} fnListener @returns {function(): void} unsubscribe */
		subscribe(fnListener) {
			this._aListeners.push(fnListener);
			this._ensureStarted();
			return () => this._unsubscribe(fnListener);
		}

		_unsubscribe(fnListener) {
			this._aListeners = this._aListeners.filter((fn) => fn !== fnListener);
			if (!this._aListeners.length) {
				this._stop();
			}
		}

		_ensureStarted() {
			if (this._oObserver || !window.MutationObserver) {
				return;
			}
			const oStaticArea = sap.ui.getCore().getStaticAreaRef();
			this._oObserver = new MutationObserver((aMutations) => {
				this._aListeners.forEach((fnListener) => fnListener(aMutations));
			});
			this._oObserver.observe(oStaticArea, { childList: true, subtree: true });
		}

		_stop() {
			if (this._oObserver) {
				this._oObserver.disconnect();
				this._oObserver = null;
			}
		}
	}

	return new DomMutationBus();
});
