/*!
 * SAPUI5
 * (c) Copyright 2025 SAP SE. All rights reserved.
 */
/**
 * Defines support rules of the SmartTable control of sap.ui.comp library.
 */
sap.ui.define([
	'sap/ui/support/library',
	'sap/base/Log',
	"sap/ui/base/Object",
	"sap/ui/table/rules/TableHelper.support"
], function(SupportLib, Log, BaseObject, SupportHelper) {
		'use strict';

		// shortcuts
		var Categories = SupportLib.Categories; // Accessibility, Performance, Memory, ...
		var Severity = SupportLib.Severity; // Hint, Warning, Error
		var Audiences = SupportLib.Audiences; // Control, Internal, Application

		//**********************************************************
		// Rule Definitions
		//**********************************************************

		/* eslint-disable no-lonely-if */

		var oSmartTableReservedKeywordsRule = {
			id: 'smartTableEntityFieldName',
			audiences: [Audiences.Application],
			categories: [Categories.DataModel],
			enabled: true,
			minversion: '1.52',
			title: 'SmartTable: Forbidden entity field name',
			description: 'The SmartTable entity uses reserved keywords as field names',
			resolution: 'Rename the field name of your OData entity that is using a reserved keyword',
			resolutionurls: [{
				text: 'API Reference: SmartTable -> properties -> entitySet ',
				href:'https://ui5.sap.com/#/api/sap.ui.comp.smarttable.SmartTable'
			}],
			check: function (oIssueManager, oCoreFacade, oScope) {
				oScope.getElementsByClassName('sap.ui.comp.smarttable.SmartTable')
					.forEach(function(oSmartTable) {
						var aReserved, sId = oSmartTable.getId();

						if (!oSmartTable._aTableViewMetadata) {
							return;
						}

						aReserved = [
							'variant',
							'btnFullScreen',
							'btnEditToggle',
							'header',
							'toolbarSeperator',
							'toolbarSpacer',
							'btnPersonalisation',
							'btnExcelExport',
							'persoController',
							'ui5table',
							'infoToolbarText'
						];

						oSmartTable._aTableViewMetadata.forEach(function (oField) {
							if (aReserved.indexOf(oField.name) > -1) {
								oIssueManager.addIssue({
									severity: Severity.High,
									details: 'SmartTable ' + sId + ' is assigned to an entity that is using a reserved keyword as field name. Please rename field ' + oField.name,
									context: {
										id: sId
									}
								});
							}
						});
					});
			}
		};

		var oSmartTableRebindTableBeforeInitialise = {
			id: "smartTableRebindTableBeforeInitialise",
			audiences: [Audiences.Application],
			categories: [Categories.Usage],
			enabled: true,
			minversion: '1.30',
			title: 'SmartTable: The rebindTable method usage',
			description: 'The call to the rebindTable method was done before the SmartTable control is initialized',
			resolution: 'Applications can listen to the "initialise" event or "isInitialised" method of the SmartTable and then call the rebindTable method. This ensures that the SmartTable control can correctly create and update the binding for the inner table',
			resolutionurls: [{
				text: 'API Reference: initialise event',
				href: 'https://ui5.sap.com/#/api/sap.ui.comp.smarttable.SmartTable/events/initialise'
			}, {
				text: 'API Reference: isInitialised method',
				href: 'https://ui5.sap.com/#/api/sap.ui.comp.smarttable.SmartTable/methods/isInitialised'
			}],
			check: function(oIssueManager, oCoreFacade, oScope) {
				var aRelevantLogEntries = Log.getLogEntries().filter(function(oLogEntry) {
					return oLogEntry.component == "sap.ui.comp.smarttable.SmartTable";
				});

				oScope.getElementsByClassName("sap.ui.comp.smarttable.SmartTable").forEach(function(oSmartTable) {
					var sId = oSmartTable.getId();
					var oControlSpecificErrorLog = aRelevantLogEntries.find(function(oErrorLog) {
						return oErrorLog.details == sId && oErrorLog.message.indexOf("rebindTable method called before the SmartTable is initialized") > -1;
					});

					if (oControlSpecificErrorLog) {
						oIssueManager.addIssue({
							severity: Severity.High,
							details: 'The rebindTable method is called before the SmartTable with id: ' + sId + ' is initialized',
							context: {
								id: sId
							}
						});
					}
				});
			}
		};

	/*
	 * Check for default ODataModel
	 */
	var oSmartTableModelBindingRule = {
		id: "smartTableModelBinding",
		categories: [Categories.Bindings],
		title: "SmartTable: Model and Binding",
		description: "Checks whether the default/unnamed model is present and is an ODataModel and if the binding makes use of this model",
		resolution: "Ensure that the desired ODataModel is set as an unnamed/default model on the control/view and is used in the binding accordingly",
		minversion: "1.46",
		check: function(oIssueManager, oCoreFacade, oScope) {
			var aSmartTables = SupportHelper.find(oScope, true, "sap/ui/comp/smarttable/SmartTable");
			var i, iLen = aSmartTables.length, oSmartTable, oModel, oInfo;
			for (i = 0; i < iLen; i++) {
				oSmartTable = aSmartTables[i];
				if (oSmartTable) {
					oModel = oSmartTable.getModel();
					// Check whether default model exists
					if (!oModel) {
						SupportHelper.reportIssue(oIssueManager, "The SmartTable expects a default/unnamed model to be present", Severity.Medium, oSmartTable.getId());
					}
					// Check if default model is an ODataModel (old/v2)
					if (!BaseObject.isObjectA(oModel, "sap.ui.model.odata.ODataModel") && !BaseObject.isObjectA(oModel, "sap.ui.model.odata.v2.ODataModel")) {
						SupportHelper.reportIssue(oIssueManager, "ODataModel should be used as the default/unnamed model", Severity.Medium, oSmartTable.getId());
					}
					// Check if binding on the inner UI5 table is done for an unnamed model
					oInfo = oSmartTable.getTable().getBindingInfo(oSmartTable._sAggregation);
					if (oInfo.model) {
						SupportHelper.reportIssue(oIssueManager, "For binding rows/items of the table in the SmartTable an unnamed (default) model should be used", Severity.Medium, oSmartTable.getId());
					}
				}
			}
		}
	};

	/*
	 * Check for default ODataModel
	 */
	var oSmartTableDeprecatedModelRule = {
		id: "smartTableDeprecatedModel",
		categories: [Categories.DataModel],
		title: "SmartTable: Deprecated Model",
		description: "Checks whether the model is a sap/ui/model/odata/ODataModel as this has been deprecated since version 1.48",
		resolution: "Use a sap/ui/model/odata/v2/ODataModel as default/unnamed model to ensure that SmartTable built-in functionality is fully available",
		minversion: "1.48",
		check: function(oIssueManager, oCoreFacade, oScope) {
			var aSmartTables = SupportHelper.find(oScope, true, "sap/ui/comp/smarttable/SmartTable");
			var i, iLen = aSmartTables.length, oSmartTable, oModel;
			for (i = 0; i < iLen; i++) {
				oSmartTable = aSmartTables[i];
				if (oSmartTable) {
					oModel = oSmartTable.getModel();
					// Check if default model is an ODataModel (old)
					if (BaseObject.isObjectA(oModel, "sap.ui.model.odata.ODataModel")) {
						SupportHelper.reportIssue(oIssueManager, "Deprecated ODataModel should not be used", Severity.Medium, oSmartTable.getId());
					}
				}
			}
		}
	};

	//**********************************************************
	// Helper Functions for Custom Column Validation
	//**********************************************************

	/**
	 * Validates if a columnKey follows the UI5 ID format requirements
	 * @param {string} sColumnKey - The column key to validate
	 * @returns {boolean} - True if valid, false otherwise
	 */
	function isValidColumnKey(sColumnKey) {
		if (!sColumnKey || typeof sColumnKey !== "string") {
			return false;
		}
		// UI5 ID format: must start with letter or underscore, followed by letters, digits, underscores, dashes, periods, or colons
		var rValidId = /^[a-zA-Z_][a-zA-Z0-9_\-.:]*$/;
		return rValidId.test(sColumnKey);
	}

	/**
	 * Validates property combinations that should be set together
	 * @param {object} oP13nData - The p13n data object
	 * @returns {array} - Array of validation issues
	 */
	function validatePropertyCombinations(oP13nData) {
		const aIssues = [];
		let aAdditionalProps;

		// description + displayBehaviour should be used together
		if (oP13nData.description && !oP13nData.displayBehaviour) {
			aIssues.push({
				severity: "Medium",
				message: "Property 'description' is set but 'displayBehaviour' is missing. Set displayBehaviour to control how ID and description are displayed (e.g., 'idOnly', 'descriptionOnly', 'idAndDescription', 'descriptionAndId')."
			});
		}

		// unit + isCurrency for currency columns
		if (oP13nData.unit && oP13nData.isCurrency === false) {
			aIssues.push({
				severity: "Medium",
				message: "Property 'unit' is set but 'isCurrency' is false. This may cause incorrect formatting for currency columns."
			});
		}

		// additionalProperty should include all referenced properties
		if (oP13nData.description && oP13nData.additionalProperty) {
			aAdditionalProps = oP13nData.additionalProperty.split(",").map(function(s) { return s.trim(); });
			if (aAdditionalProps.indexOf(oP13nData.description) === -1) {
				aIssues.push({
					severity: "High",
					message: "Property 'description' ('" + oP13nData.description + "') should be included in 'additionalProperty' to ensure it's loaded from the backend."
				});
			}
		}

		// typeInstance + validation properties should be used together
		if (oP13nData.typeInstance && (!oP13nData.maxLength && !oP13nData.precision && !oP13nData.scale)) {
			aIssues.push({
				severity: "Low",
				message: "Property 'typeInstance' is set but validation properties (maxLength, precision, scale) are missing. Consider adding validation constraints for better data handling."
			});
		}

		// sortProperty + additionalSortProperty for multi-field sorting
		if (oP13nData.additionalSortProperty && !oP13nData.sortProperty) {
			aIssues.push({
				severity: "Medium",
				message: "Property 'additionalSortProperty' is set but 'sortProperty' is missing. Set sortProperty as the main sort field."
			});
		}

		// additionalSortProperty properties should be in additionalProperty
		if (oP13nData.additionalSortProperty && oP13nData.additionalProperty) {
			var aAdditionalSortProps = oP13nData.additionalSortProperty.split(",").map(function(s) { return s.trim(); });
			aAdditionalProps = oP13nData.additionalProperty.split(",").map(function(s) { return s.trim(); });
			aAdditionalSortProps.forEach(function(sSortProp) {
				if (aAdditionalProps.indexOf(sSortProp) === -1) {
					aIssues.push({
						severity: "High",
						message: "Property '" + sSortProp + "' from 'additionalSortProperty' should be included in 'additionalProperty' to ensure it's loaded from the backend."
					});
				}
			});
		}

		// unit property should be in additionalProperty
		if (oP13nData.unit && oP13nData.additionalProperty) {
			aAdditionalProps = oP13nData.additionalProperty.split(",").map(function(s) { return s.trim(); });
			if (aAdditionalProps.indexOf(oP13nData.unit) === -1) {
				aIssues.push({
					severity: "High",
					message: "Property 'unit' ('" + oP13nData.unit + "') should be included in 'additionalProperty' to ensure unit information is loaded from the backend."
				});
			}
		}

		return aIssues;
	}

	/**
	 * Validates conflicting property combinations
	 * @param {object} oP13nData - The p13n data object
	 * @returns {array} - Array of validation issues
	 */
	function validateConflictingProperties(oP13nData) {
		var aIssues = [];

		// ignorePaste + customParseFunction conflict
		if (oP13nData.ignorePaste === true && oP13nData.customParseFunction) {
			aIssues.push({
				severity: "High",
				message: "Properties 'ignorePaste' and 'customParseFunction' conflict. If ignorePaste is true, customParseFunction will not be called."
			});
		}

		// isGroupable + aggregationRole: "measure" conflict
		if (oP13nData.isGroupable === true && oP13nData.aggregationRole === "measure") {
			aIssues.push({
				severity: "High",
				message: "Properties 'isGroupable' and 'aggregationRole: \"measure\"' conflict. Grouping is only meaningful for dimensions, not measures."
			});
		}

		// width + autoColumnWidth conflict
		if (oP13nData.width && oP13nData.autoColumnWidth) {
			aIssues.push({
				severity: "Medium",
				message: "Properties 'width' and 'autoColumnWidth' conflict. If width is set, autoColumnWidth is ignored."
			});
		}

		// Additional side effect validations from documentation

		// description + sortProperty may confuse users
		if (oP13nData.description && oP13nData.sortProperty === oP13nData.description) {
			aIssues.push({
				severity: "Low",
				message: "Sorting by description may confuse users if the main value is an ID. Consider sorting by the main property instead."
			});
		}

		// nullable: false without default values
		if (oP13nData.nullable === false && !oP13nData.defaultValue) {
			aIssues.push({
				severity: "Medium",
				message: "Property 'nullable' is false but no default value is provided. Paste/import operations may fail without a default value."
			});
		}

		// columnIndex + dynamic personalization conflict
		if (oP13nData.columnIndex !== undefined) {
			aIssues.push({
				severity: "Low",
				message: "Fixed 'columnIndex' may conflict with user-driven column reordering in personalization. Consider if this is intended."
			});
		}

		return aIssues;
	}

	/**
	 * Validates data type and formatting properties
	 * @param {object} oP13nData - The p13n data object
	 * @returns {array} - Array of validation issues
	 */
	function validateDataTypeProperties(oP13nData) {
		var aIssues = [];

		// Validate edmType format
		if (oP13nData.edmType && typeof oP13nData.edmType === "string") {
			var aValidEdmTypes = [
				"Edm.String", "Edm.Int16", "Edm.Int32", "Edm.Int64", "Edm.Decimal", "Edm.Double", "Edm.Single",
				"Edm.Boolean", "Edm.DateTime", "Edm.DateTimeOffset", "Edm.Time", "Edm.Guid", "Edm.Binary"
			];
			if (aValidEdmTypes.indexOf(oP13nData.edmType) === -1) {
				aIssues.push({
					severity: "Medium",
					message: "Property 'edmType' has invalid value '" + oP13nData.edmType + "'. Expected one of: " + aValidEdmTypes.join(", ")
				});
			}
		}

		// Validate precision for DateTimeOffset
		if (oP13nData.edmType === "Edm.DateTimeOffset" && oP13nData.precision) {
			if (typeof oP13nData.precision !== "number" || oP13nData.precision < 0 || oP13nData.precision > 7) {
				aIssues.push({
					severity: "Medium",
					message: "Property 'precision' for Edm.DateTimeOffset should be a number between 0 and 7 (fractional seconds digits)."
				});
			}
		}

		// Validate scale and precision relationship
		if (oP13nData.scale && oP13nData.precision) {
			if (typeof oP13nData.scale === "number" && typeof oP13nData.precision === "number") {
				if (oP13nData.scale > oP13nData.precision) {
					aIssues.push({
						severity: "High",
						message: "Property 'scale' (" + oP13nData.scale + ") cannot be greater than 'precision' (" + oP13nData.precision + ")."
					});
				}
			}
		}

		return aIssues;
	}

	/**
	 * Check p13nData for custom columns with comprehensive validation.
	 */
	var oSmartTableCustomColumnP13nData = {
		id: "smartTableCustomColumnPersonalizationData",
		audiences: [
			Audiences.Application
		],
		categories: [
			Categories.Consistency
		],
		minversion: "1.44",
		async: false,
		title: "SmartTable: Custom Column p13nData Configuration",
		description: "Validates comprehensive p13nData configuration for custom columns including required properties, property combinations, conflicts, and data type constraints",
		resolution: "Ensure correct p13nData configuration for custom columns. Required properties: columnKey, leadingProperty, text. Check for property conflicts and proper data type settings. See CustomColumn.md documentation for detailed guidelines.",
		resolutionurls: [
			{
				text: "API FAQ: SmartTable",
				href: "https://ui5.sap.com/#/api/sap.ui.comp.smarttable.SmartTable/faq"
			},
			{
				text: "Custom Column Documentation (FAQ Section 1)",
				href: "https://ui5.sap.com/#/api/sap.ui.comp.smarttable.SmartTable%23faq"
			}
		],
		check: function(oIssueManager, oCoreFacade, oScope) {
			oScope.getElementsByClassName("sap.ui.comp.smarttable.SmartTable").forEach(function(oSmartTable) {
				var aCustomColumnKeys = oSmartTable._aExistingColumns;
				if (aCustomColumnKeys && aCustomColumnKeys.length) {
					aCustomColumnKeys.forEach(function(sColumnKey) {
						var oColumn = oSmartTable._getColumnByKey(sColumnKey);
						if (!oColumn) {
							return;
						}

						var oP13nData = oColumn.data("p13nData");
						if (!oP13nData) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "Custom column is missing p13nData configuration. Add p13nData with at least columnKey, leadingProperty, and text properties.",
								context: {
									id: oColumn.getId()
								}
							});
							return;
						}

						// Core required properties validation
						if (!oP13nData.hasOwnProperty("columnKey")) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData columnKey configuration is not defined. This is required for personalization.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (!isValidColumnKey(oP13nData.columnKey)) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData columnKey '" + oP13nData.columnKey + "' has invalid format. Must start with letter or underscore, followed by letters, digits, underscores, dashes, periods, or colons.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						if (!oP13nData.hasOwnProperty("leadingProperty") && !oColumn.isA("sap.ui.table.AnalyticalColumn")) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData leadingProperty configuration is not defined. This is required for sorting, filtering, and export.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (!oColumn.isA("sap.ui.table.AnalyticalColumn") && typeof (oP13nData.leadingProperty) !== "string") {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData leadingProperty configuration only supports string value.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (typeof oP13nData.leadingProperty == "string" && oP13nData.leadingProperty.split(",").length > 1) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData leadingProperty configuration cannot have multiple values.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (oColumn.isA("sap.ui.table.AnalyticalColumn") && !oColumn.getLeadingProperty()) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The leadingProperty must be defined on the AnalyticalColumn.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Type validation - enhanced
						if (!oP13nData.hasOwnProperty("type")) {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData type configuration is not defined. This is required for proper filtering and export behavior.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (oP13nData.type && typeof oP13nData.type !== "string") {
							oIssueManager.addIssue({
								severity: Severity.High,
								details: "The p13nData type configuration must be a string value (e.g., 'string', 'numeric', 'date', 'boolean').",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Additional property validation - enhanced
						if (oP13nData.hasOwnProperty("additionalProperty")) {
							if (typeof (oP13nData.additionalProperty) !== "string") {
								oIssueManager.addIssue({
									severity: Severity.High,
									details: "The p13nData additionalProperty configuration only supports a string with comma separated values.",
									context: {
										id: oColumn.getId()
									}
								});
							}

							if (oP13nData.additionalProperty === oP13nData.leadingProperty) {
								oIssueManager.addIssue({
									severity: Severity.High,
									details: "The p13nData additionalProperty and leadingProperty must not be equal.",
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// Sorting and filtering validation
						if (oColumn.isA("sap.m.Column") && !oP13nData.hasOwnProperty("sortProperty")) {
							oIssueManager.addIssue({
								severity: Severity.Low,
								details: "The p13nData sortProperty configuration is not defined. This may limit sorting functionality.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (!oColumn.isA("sap.m.Column") && !oColumn.getProperty("sortProperty")) {
							oIssueManager.addIssue({
								severity: Severity.Low,
								details: "The sortProperty on the column is not defined. This may limit sorting functionality.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						if (oColumn.isA("sap.m.Column") && !oP13nData.hasOwnProperty("filterProperty")) {
							oIssueManager.addIssue({
								severity: Severity.Low,
								details: "The p13nData filterProperty configuration is not defined. This may limit filtering functionality.",
								context: {
									id: oColumn.getId()
								}
							});
						} else if (!oColumn.isA("sap.m.Column") && !oColumn.getProperty("filterProperty")) {
							oIssueManager.addIssue({
								severity: Severity.Low,
								details: "The filterProperty on the column is not defined. This may limit filtering functionality.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Validation constraints validation
						if (oP13nData.hasOwnProperty("maxLength") && (typeof oP13nData.maxLength !== "number" || oP13nData.maxLength <= 0)) {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData maxLength must be a positive number.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						if (oP13nData.hasOwnProperty("scale") && (typeof oP13nData.scale !== "number" || oP13nData.scale < 0)) {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData scale must be a non-negative number.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Width validation
						if (oP13nData.hasOwnProperty("width") && oP13nData.hasOwnProperty("autoColumnWidth")) {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "Properties 'width' and 'autoColumnWidth' should not be set together. If width is set, autoColumnWidth is ignored.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Additional property validations from CustomColumn.md

						// columnIndex validation
						if (oP13nData.hasOwnProperty("columnIndex") && (typeof oP13nData.columnIndex !== "number" || oP13nData.columnIndex < 0)) {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData columnIndex must be a non-negative number.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// nullable validation
						if (oP13nData.hasOwnProperty("nullable") && typeof oP13nData.nullable !== "boolean" && oP13nData.nullable !== "false") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData nullable must be a boolean value or the string 'false'.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// align validation
						if (oP13nData.hasOwnProperty("align")) {
							var aValidAlignValues = ["Begin", "Center", "End", "Left", "Right", "Initial"];
							if (typeof oP13nData.align !== "string" || aValidAlignValues.indexOf(oP13nData.align) === -1) {
								oIssueManager.addIssue({
									severity: Severity.Medium,
									details: "The p13nData align must be one of: " + aValidAlignValues.join(", "),
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// isDigitSequence validation
						if (oP13nData.hasOwnProperty("isDigitSequence") && typeof oP13nData.isDigitSequence !== "boolean") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData isDigitSequence must be a boolean value.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// displayBehaviour validation
						if (oP13nData.hasOwnProperty("displayBehaviour")) {
							var aValidDisplayBehaviours = ["idOnly", "descriptionOnly", "idAndDescription", "descriptionAndId"];
							if (typeof oP13nData.displayBehaviour !== "string" || aValidDisplayBehaviours.indexOf(oP13nData.displayBehaviour) === -1) {
								oIssueManager.addIssue({
									severity: Severity.Medium,
									details: "The p13nData displayBehaviour must be one of: " + aValidDisplayBehaviours.join(", "),
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// timezone validation
						if (oP13nData.hasOwnProperty("timezone") && typeof oP13nData.timezone !== "string") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData timezone must be a string value (e.g., 'UTC', 'Europe/Berlin').",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// isTimezone validation
						if (oP13nData.hasOwnProperty("isTimezone") && typeof oP13nData.isTimezone !== "boolean") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData isTimezone must be a boolean value.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// isCurrency validation
						if (oP13nData.hasOwnProperty("isCurrency") && typeof oP13nData.isCurrency !== "boolean") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData isCurrency must be a boolean value.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// isGroupable validation
						if (oP13nData.hasOwnProperty("isGroupable") && typeof oP13nData.isGroupable !== "boolean") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData isGroupable must be a boolean value.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// aggregationRole validation
						if (oP13nData.hasOwnProperty("aggregationRole")) {
							var aValidAggregationRoles = ["dimension", "measure"];
							if (typeof oP13nData.aggregationRole !== "string" || aValidAggregationRoles.indexOf(oP13nData.aggregationRole) === -1) {
								oIssueManager.addIssue({
									severity: Severity.Medium,
									details: "The p13nData aggregationRole must be either 'dimension' or 'measure'.",
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// ignorePaste validation
						if (oP13nData.hasOwnProperty("ignorePaste") && typeof oP13nData.ignorePaste !== "boolean") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData ignorePaste must be a boolean value.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// additionalSortProperty validation
						if (oP13nData.hasOwnProperty("additionalSortProperty") && typeof oP13nData.additionalSortProperty !== "string") {
							oIssueManager.addIssue({
								severity: Severity.Medium,
								details: "The p13nData additionalSortProperty must be a string with comma-separated property names.",
								context: {
									id: oColumn.getId()
								}
							});
						}

						// Check for invalid separators in additionalProperty and additionalSortProperty
						["additionalProperty", "additionalSortProperty"].forEach(function(sKey) {
							if (oP13nData.hasOwnProperty(sKey) && typeof oP13nData[sKey] === "string") {
								// Only allow comma as separator, no semicolon, pipe, or whitespace as separator
								if (/[;|\/\\\s&]/.test(oP13nData[sKey])) {
									oIssueManager.addIssue({
										severity: Severity.High,
										details: "The p13nData " + sKey + " contains an invalid separator. Only commas (,) are allowed as separators.",
										context: {
											id: oColumn.getId()
										}
									});
								}
							}
						});

						// autoColumnWidth validation
						if (oP13nData.hasOwnProperty("autoColumnWidth")) {
							if (typeof oP13nData.autoColumnWidth !== "boolean" && typeof oP13nData.autoColumnWidth !== "object") {
								oIssueManager.addIssue({
									severity: Severity.Medium,
									details: "The p13nData autoColumnWidth must be a boolean or an object with configuration properties.",
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// type validation - enhanced with valid values
						if (oP13nData.hasOwnProperty("type") && typeof oP13nData.type === "string") {
							var aValidTypes = ["date", "time", "boolean", "numeric", "stringdate", "string"];
							if (aValidTypes.indexOf(oP13nData.type) === -1) {
								oIssueManager.addIssue({
									severity: Severity.Medium,
									details: "The p13nData type should be one of: " + aValidTypes.join(", "),
									context: {
										id: oColumn.getId()
									}
								});
							}
						}

						// Use helper functions for comprehensive validation
						var aCombinationIssues = validatePropertyCombinations(oP13nData);
						aCombinationIssues.forEach(function(oIssue) {
							oIssueManager.addIssue({
								severity: oIssue.severity,
								details: oIssue.message,
								context: {
									id: oColumn.getId()
								}
							});
						});

						var aConflictIssues = validateConflictingProperties(oP13nData);
						aConflictIssues.forEach(function(oIssue) {
							oIssueManager.addIssue({
								severity: oIssue.severity,
								details: oIssue.message,
								context: {
									id: oColumn.getId()
								}
							});
						});

						var aDataTypeIssues = validateDataTypeProperties(oP13nData);
						aDataTypeIssues.forEach(function(oIssue) {
							oIssueManager.addIssue({
								severity: oIssue.severity,
								details: oIssue.message,
								context: {
									id: oColumn.getId()
								}
							});
						});
					});
				}
			});
		}
	};

		return [
			oSmartTableReservedKeywordsRule,
			oSmartTableRebindTableBeforeInitialise,
			oSmartTableModelBindingRule,
			/** @deprecated As of version 1.48, together with the ODataModel (v1) */
			oSmartTableDeprecatedModelRule,
			oSmartTableCustomColumnP13nData
		];

	}, true);
