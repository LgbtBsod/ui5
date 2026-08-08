sap.ui.define([], function () {
	"use strict";

	class ODataContextUtils {
		static isTransient(oContext) {
			if (!oContext) {
				return false;
			}
			return typeof oContext.isTransient === "function" ? oContext.isTransient() : !!oContext.bCreated;
		}
	}

	return ODataContextUtils;
});
