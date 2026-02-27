/*!
 * SAPUI5
 * (c) Copyright 2025 SAP SE. All rights reserved.
 */

// Provides control sap.m.P13nSortItem.
sap.ui.define([
	'sap/m/library', 'sap/ui/core/Item'
], function(library, Item) {
	"use strict";

	/**
	 * Constructor for a new P13nSortItem.
	 *
	 * @param {string} [sId] ID for the new control, generated automatically if no ID is given
	 * @param {object} [mSettings] initial settings for the new control
	 * @class Type for <code>sortItems</code> aggregation in P13nSortPanel control.
	 * @extends sap.ui.core.Item
	 * @version 1.144.0
	 * @constructor
	 * @private
	 * @since 1.144.0
	 * @alias sap.ui.comp.p13n.P13nSortItem
	 */
	var P13nSortItem = Item.extend("sap.ui.comp.p13n.P13nSortItem", /** @lends sap.ui.comp.p13n.P13nSortItem.prototype */
		{
			metadata: {

				library: "sap.ui.comp",
				properties: {

					/**
					 * sap.m.P13nConditionOperation
					 */
					operation: {
						type: "string",
						group: "Misc",
						defaultValue: null
					},

					/**
					 * key of the column
					 */
					columnKey: {
						type: "string",
						group: "Misc",
						defaultValue: null
					}
				}
			}
		});

	P13nSortItem.prototype.setColumnKey = function(v) {
		return this.setProperty("columnKey", v, true);
	};

	P13nSortItem.prototype.setOperation = function(v) {
		return this.setProperty("operation", v, true);
	};

	return P13nSortItem;

});
