sap.ui.define(["sap/ui/model/resource/ResourceModel"], function (ResourceModel) {
	"use strict";

	const oModel = new ResourceModel({
		bundleName: "sap.pc_lite.check.i18n.i18n"
	});
	const oBundle = oModel.getResourceBundle();

	class I18n {
		static getText(sKey, aArgs) {
			return oBundle.getText(sKey, aArgs);
		}
		static getModel() {
			return oModel;
		}
	}

	return I18n;
});
