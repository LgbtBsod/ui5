/*!
 * SAPUI5
 * (c) Copyright 2025 SAP SE. All rights reserved.
 */

// ----------------------------------------------------------------
// Utility class used by smart controls for multi-unit scenario
// ----------------------------------------------------------------
sap.ui.define([
	"sap/m/table/Util",
	"sap/ui/core/Element",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"sap/base/util/merge"
], function(
	Util,
	Element,
	Filter,
	FilterOperator,
	merge
) {
	"use strict";

	/**
	 * Utility class used by smart controls for multi-unit scenario
	 *
	 * This module is only for internal use!
	 *
	 * @private
	 */
	const MultiUnitUtil = {
		/**
		 * returns true/false based on whether multi-unit "*" value is present for unit
		 *
		 * @private
		 * @param {string} sUnit - The unit value
		 * @returns {function} whether there are multiple units - "*"
		 */
		isMultiUnit: function(sUnit) {
			return sUnit === "*";
		},
		/**
		 * returns true/false based on whether multi-unit "*" value is present for unit
		 *
		 * @private
		 * @param {string} sUnit - The unit value
		 * @returns {function} whether there are no multiple units - "*"
		 */
		isNotMultiUnit: function(sUnit) {
			return sUnit !== "*";
		},
		openMultiUnitPopover: async function(oEvent, mAdditionalParams) {
			const oSmartTable = Element.getElementById(mAdditionalParams.smartTableId);
			const oAnalyticalTable = oSmartTable.getTable();
			const oBinding = oAnalyticalTable.getBinding("rows");
			const sValue = mAdditionalParams.value;
			const sUnit = mAdditionalParams.unit;
			let oAnalyticalInfoForColumn, sDimension;

			// no binding or value or unit --> return
			if (!oBinding || !sValue || !sUnit) {
				return;
			}

			const oLink = oEvent.getSource();
			// The link is inside a container (e.g. VBox), get this layout container control in order to get the row and finally the analytical info
			let oLayout = oLink.getParent();

			if (mAdditionalParams.additionalParent) {
				oLayout = oLayout.getParent();
			}
			// via the row, we can get the analytical information
			const oAnalyticalInfo = oAnalyticalTable.getAnalyticalInfoOfRow(oLayout.getParent());

			if (!oAnalyticalInfo) {
				return;
			}
			// prepare filter statement, select and title

			let aFilters = [];
			const aSelect = [
				// always request value and unit
				sValue, sUnit
			];

			// Add any application filters already present on the binding (these should be the ones already processed by SmartTable)
			if (oBinding.aApplicationFilter) {
				aFilters = [].concat(oBinding.aApplicationFilter);
			}
			// Get custom query parameters (e.g. "search" from the parent binding and apply it here!)
			const mBindingInfo = oAnalyticalTable.getBindingInfo("rows");
			const mCustomParameters = (mBindingInfo && mBindingInfo.parameters && mBindingInfo.parameters.custom) ? merge({}, mBindingInfo.parameters.custom) : undefined;

			// Grand Total --> do nothing as we already add Currency and unit to the Select clause
			if (oAnalyticalInfo.groupTotal || oAnalyticalInfo.group) {
				// Group Total / Group Header
				const aGroupedColumns = oAnalyticalInfo.groupedColumns;

				for (let i = 0; i < aGroupedColumns.length; i++) {
					sDimension = Element.getElementById(aGroupedColumns[i]).getLeadingProperty();
					if (!sDimension) {
						continue;
					}
					// Get Analytical Info for column --> in order to determine/use the proper dimensionProperty!
					// When grouping is done on text column, the actual grouping happens on the dimension (code) property and not the text
					oAnalyticalInfoForColumn = oBinding.getAnalyticalInfoForColumn(sDimension);
					if (oAnalyticalInfoForColumn) {
						sDimension = oAnalyticalInfoForColumn.dimensionPropertyName;
					}
					if (sDimension) {
						aFilters.push(new Filter(sDimension, FilterOperator.EQ, oAnalyticalInfo.context.getProperty(sDimension)));
					}
				}
			} else if (!oAnalyticalInfo.grandTotal) {
				// Line item that contains multiple units
				const aProperties = Object.getOwnPropertyNames(oBinding.getDimensionDetails());
				for (let i = 0; i < aProperties.length; i++) {
					aFilters.push(new Filter(aProperties[i], FilterOperator.EQ, oAnalyticalInfo.context.getProperty(aProperties[i])));
				}
			}

			let oPopover = Element.getElementById(mAdditionalParams.smartTableId + "-multiUnitPopover");
			const bPopoverAlreadyExists = (oPopover ? true : false);
			const sPopoverId = mAdditionalParams.smartTableId + "-multiUnitPopover";
			//If there is no popover, use the table id for the new popover control
			const vPopover = oPopover ? oPopover : sPopoverId;
			const oItemsBindingInfo = {
				path: oBinding.getPath(),
				filters: aFilters,
				parameters: {
					select: aSelect.join(","),
					custom: mCustomParameters
				}
			};
			const mSettings = {
				control: oAnalyticalTable,
				grandTotal: oAnalyticalInfo.grandTotal,
				itemsBindingInfo: oItemsBindingInfo,
				listItemContentTemplate: mAdditionalParams.template
			};

			oPopover = await Util.createOrUpdateMultiUnitPopover(vPopover, mSettings);

			if (!bPopoverAlreadyExists) {
				oAnalyticalTable.addDependent(oPopover);
			}

			oPopover.openBy(oLink);
		}
	};

	return MultiUnitUtil;

}, /* bExport= */true);
