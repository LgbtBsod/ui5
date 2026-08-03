sap.ui.define([], function () {
	"use strict";

	/** Tracks pending setTimeout ids so they can all be cancelled together (e.g. on destroy()). */
	class TimeoutTracker {
		constructor() {
			this._aPending = [];
		}

		/** @returns {number} the timeout id, also tracked internally. */
		schedule(fnCallback, iDelay) {
			const iId = setTimeout(() => {
				this._aPending = this._aPending.filter((id) => id !== iId);
				fnCallback();
			}, iDelay);
			this._aPending.push(iId);
			return iId;
		}

		clearAll() {
			this._aPending.forEach(clearTimeout);
			this._aPending = [];
		}
	}

	return TimeoutTracker;
});
