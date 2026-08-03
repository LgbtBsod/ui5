sap.ui.define([
	"./Constants", "./TransientOnce", "./ODataUtils", "sap/ui/model/Filter", "sap/ui/model/FilterOperator"
], function (Constants, TransientOnce, ODataUtils, Filter, FilterOperator) {
	"use strict";

	const F = Constants.FIELDS;

	// Date/Time/Timezone проецируются с to_Basic прямо на Root (см.
	// ZI_CheckRoot.ddls.asddls), поэтому пишем на путь заголовка, без навигации.
	class DateTimeAutofill {
		constructor() {
			this._oOnce = new TransientOnce();
		}

		applyIfNeeded(oView, fnIsTransient) {
			this._oOnce.runOnce(oView, fnIsTransient, (oContext, sPath) => {
				const oModel = oView.getModel();
				if (!oContext.getProperty(F.BASIC_DATE)) {
					oModel.setProperty(`${sPath}/${F.BASIC_DATE}`, new Date());
				}
				if (!oContext.getProperty(F.BASIC_TIME)) {
					oModel.setProperty(`${sPath}/${F.BASIC_TIME}`, DateTimeAutofill._buildEdmTime(new Date()));
				}
				if (!oContext.getProperty(F.TIMEZONE)) {
					const sTimezone = DateTimeAutofill._resolveBrowserTimezone();
					if (sTimezone) {
						oModel.setProperty(`${sPath}/${F.TIMEZONE}`, sTimezone);
						oModel.setProperty(`${sPath}/${F.TIMEZONE_TEXT}`, sTimezone);
						DateTimeAutofill._resolveTimezoneText(oModel, sPath, sTimezone);
					}
				}
			});
		}

		static _resolveTimezoneText(oModel, sPath, sTimezone) {
			ODataUtils.readEntitySet(oModel, `/${Constants.ENTITY_SETS.TIME_ZONES}`, {
				filters: [new Filter("TimeZoneCode", FilterOperator.EQ, sTimezone)],
				urlParameters: { "$select": "TimeZoneText" }
			}).then((aResults) => {
				if (aResults.length && oModel.getProperty(`${sPath}/${F.TIMEZONE}`) === sTimezone) {
					oModel.setProperty(`${sPath}/${F.TIMEZONE_TEXT}`, aResults[0].TimeZoneText);
				}
			});
		}

		static _buildEdmTime(oDate) {
			const iMs = ((oDate.getHours() * 60 + oDate.getMinutes()) * 60 + oDate.getSeconds()) * 1000
				+ oDate.getMilliseconds();
			return { ms: iMs, __edmType: Constants.EDM_TYPES.TIME };
		}

		static _resolveBrowserTimezone() {
			try {
				return Intl.DateTimeFormat().resolvedOptions().timeZone;
			} catch (e) {
				return "";
			}
		}
	}

	return DateTimeAutofill;
});
