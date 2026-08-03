sap.ui.define([], function () {
	"use strict";

	class ODataUtils {
		static readEntitySet(oModel, sPath, oOptions) {
			const oOpts = oOptions || {};
			return new Promise((resolve) => {
				oModel.read(sPath, {
					filters: oOpts.filters,
					urlParameters: oOpts.urlParameters,
					success: (oData) => resolve((oData && oData.results) || []),
					error: (oError) => {
						if (oOpts.onError) {
							oOpts.onError(oError);
						}
						resolve([]);
					}
				});
			});
		}
	}

	return ODataUtils;
});
