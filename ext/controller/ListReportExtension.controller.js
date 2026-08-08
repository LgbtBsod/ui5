sap.ui.define([
	"sap/ui/core/mvc/ControllerExtension",
	"../util/Constants",
	"../util/FioriElementsDom",
	"../util/ExcelExport",
	"../util/ValueHelpAutoApply",
	"../util/LocationPicker",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator"
], function (ControllerExtension, Constants, FioriElementsDom, ExcelExport, ValueHelpAutoApply, LocationPicker, Filter, FilterOperator) {
	"use strict";

	return ControllerExtension.extend("sap.pc_lite.check.ext.controller.ListReportExtension", {

		override: {
			onInit: function () {
				this._oValueHelpAutoApply = new ValueHelpAutoApply();
				this._bSmartTableSetupDone = false;
			},
			onAfterRendering: function () {
				this._scanTick();
			},
			onExit: function () {
				this._oValueHelpAutoApply.destroy();
			}
		},

		_scanTick: function () {
			const oView = this.getView();
			if (!this._bSmartTableSetupDone) {
				this._bSmartTableSetupDone = this._setupSmartTable(oView);
			}
			this._oValueHelpAutoApply.scan(oView, LocationPicker.pickHandler.bind(LocationPicker));
		},

		_setupSmartTable: function (oView) {
			const oSmartTable = FioriElementsDom.findElement(
				"sap.ui.comp.smarttable.SmartTable", () => true, oView
			);
			if (!oSmartTable) {
				return false;
			}
			const oToolbar = FioriElementsDom.getSmartTableToolbar(oSmartTable);
			if (!oToolbar || !oToolbar.addContent) {
				return false;
			}
			ExcelExport.attachButton(oSmartTable, oToolbar);
			this._wireSublocationFilter(oView, oSmartTable);
			return true;
		},

		_wireSublocationFilter: function (oView, oSmartTable) {
			oSmartTable.attachBeforeRebindTable((oEvent) => {
				const oPick = LocationPicker.getLastSublocationPick();
				if (!oPick || !oPick.withSublocation) {
					return;
				}
				const oFilterBar = FioriElementsDom.findElement(
					"sap.ui.comp.smartfilterbar.SmartFilterBar", () => true, oView
				);
				const oLocationControl = oFilterBar && oFilterBar.getControlByKey
					&& oFilterBar.getControlByKey(Constants.FIELDS.LOCATION_NAME_ROOT);
				if (!oLocationControl || oLocationControl.getValue() !== oPick.value) {
					return;
				}
				const oBindingParams = oEvent.getParameter("bindingParams");
				oBindingParams.filters = (oBindingParams.filters || []).concat(
					new Filter(Constants.FIELDS.WITH_SUBLOCATION, FilterOperator.EQ, "true")
				);
			});
		}
	});
});
