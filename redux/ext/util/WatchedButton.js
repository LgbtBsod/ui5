sap.ui.define(["./CachedElementLookup", "./ButtonWatcher"], function (CachedElementLookup, ButtonWatcher) {
	"use strict";

	class WatchedButton {
		constructor(fnFind) {
			this._oLookup = new CachedElementLookup(fnFind);
			this._oWatcher = new ButtonWatcher();
		}

		get(oScope) {
			return this._oLookup.get(oScope);
		}

		watchOnce(oScope, fnOnPress) {
			return this._oWatcher.watchOnce(this.get(oScope), fnOnPress);
		}
	}

	return WatchedButton;
});
