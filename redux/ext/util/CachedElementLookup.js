sap.ui.define([], function () {
	"use strict";

	class CachedElementLookup {
		constructor(fnFind) {
			this._fnFind = fnFind;
			this._oCached = null;
		}

		get(oScope) {
			if (this._oCached && this._oCached.getParent()) {
				return this._oCached;
			}
			this._oCached = this._fnFind(oScope);
			return this._oCached;
		}
	}

	return CachedElementLookup;
});
