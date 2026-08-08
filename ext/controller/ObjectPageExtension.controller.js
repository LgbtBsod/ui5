sap.ui.define([
	"sap/ui/core/mvc/ControllerExtension",
	"../util/Constants",
	"../util/I18n",
	"../util/ODataContextUtils",
	"../util/KpiSync",
	"../util/DateTimeAutofill",
	"../util/ValueHelpAutoApply",
	"../util/LocationPicker",
	"../util/SubTableCrud",
	"../util/CreateFullScreen",
	"../util/KeepEditMode",
	"../util/FieldStateGate",
	"../util/PernrRule",
	"../util/CheckCodeCoverage",
	"../util/RequiredFieldsRule",
	"../util/IntegrationEditGuard",
	"../util/DiscardTransientOnClose",
	"../util/DeleteConfirmGuard",
	"../util/StaleDisplayFormGuard",
	"../util/EditableTransitionGate",
	"../util/SaveIntentTracker"
], function (
	ControllerExtension, Constants, I18n, ODataContextUtils,
	KpiSync, DateTimeAutofill, ValueHelpAutoApply, LocationPicker, SubTableCrud,
	CreateFullScreen, KeepEditMode, FieldStateGate, PernrRule, CheckCodeCoverage,
	RequiredFieldsRule, IntegrationEditGuard, DiscardTransientOnClose, DeleteConfirmGuard, StaleDisplayFormGuard,
	EditableTransitionGate, SaveIntentTracker
) {
	"use strict";

	const F = Constants.FIELDS;

	/** @returns {object} sub-table CRUD config for one Object Page section. */
	function buildSectionConfig(oParams) {
		return {
			navProperty: oParams.navProperty,
			typeEntitySet: oParams.typeEntitySet,
			typeField: F.CODE,
			typeFields: { code: oParams.typeCodeField, text: oParams.typeTextField, category: F.CATEGORY },
			typeLabel: I18n.getText(oParams.typeLabelKey),
			noteField: F.COMMENT,
			noteLabel: I18n.getText(oParams.noteLabelKey),
			dialogTitle: I18n.getText(oParams.dialogTitleKey),
			resultEntitySet: Constants.ENTITY_SETS.CHECK_RESULTS,
			resultField: F.RESULT,
			resultFields: { code: F.RESULT_CODE, text: F.RESULT_TEXT_REF },
			resultLabel: I18n.getText("resultLabel")
		};
	}

	const PAGE_CONFIG = {
		sections: {
			[Constants.FACET_IDS.CHECK_ROWS_SECTION]: buildSectionConfig({
				navProperty: Constants.NAV_PROPERTIES.CHECK_ROWS,
				typeEntitySet: Constants.ENTITY_SETS.CHECK_TYPES,
				typeCodeField: F.CHECK_TYPE_CODE,
				typeTextField: F.CHECK_TYPE_TEXT,
				typeLabelKey: "checkTypeLabel",
				noteLabelKey: "commentLabel",
				dialogTitleKey: "newCheckDialogTitle"
			}),
			[Constants.FACET_IDS.BARRIERS_SECTION]: buildSectionConfig({
				navProperty: Constants.NAV_PROPERTIES.BARRIERS,
				typeEntitySet: Constants.ENTITY_SETS.BARRIER_TYPES,
				typeCodeField: F.BARRIER_TYPE_CODE,
				typeTextField: F.BARRIER_TYPE_TEXT,
				typeLabelKey: "barrierTypeLabel",
				noteLabelKey: "barrierNotesLabel",
				dialogTitleKey: "newBarrierDialogTitle"
			})
		}
	};

	/**
	 * Guard registry: each entry is {guard, refresh(guard, oView, oSnapshot), destroy?}.
	 * Adding a guard means adding one entry here — not touching onInit/onExit/_scanTick.
	 */
	function buildGuardRegistry() {
		const oCodeCoverage = new CheckCodeCoverage();
		return [
			{ guard: new SubTableCrud(), refresh: (g, oView, s) => g.scan(oView, PAGE_CONFIG, s.smartTables) },
			{ guard: new DateTimeAutofill(), refresh: (g, oView) => g.applyIfNeeded(oView, ODataContextUtils.isTransient) },
			{ guard: KpiSync, refresh: (g, oView) => g.sync(oView, ODataContextUtils.isTransient) },
			{ guard: new CreateFullScreen(), refresh: (g, oView) => g.applyIfNeeded(oView, () => true), destroy: true },
			{ guard: new KeepEditMode(), refresh: (g, oView, s) => g.refresh(oView, s) },
			{ guard: new FieldStateGate("pernr", PernrRule.getIssues), refresh: (g, oView) => g.refresh(oView), destroy: true },
			{ guard: new FieldStateGate("code", (v) => oCodeCoverage.getIssues(v)), refresh: (g, oView) => g.refresh(oView), destroy: true },
			{
				// Only gate that actually blocks Save.
				guard: new FieldStateGate("required", RequiredFieldsRule.getIssues, { blocksSave: true }),
				refresh: (g, oView) => g.refresh(oView),
				destroy: true
			},
			{ guard: new IntegrationEditGuard(), refresh: (g, oView) => g.refresh(oView) },
			{ guard: new DiscardTransientOnClose(), refresh: (g, oView, s) => g.refresh(oView, s), destroy: true },
			{ guard: new DeleteConfirmGuard(), refresh: (g, oView, s) => g.refresh(oView, s.buttons), destroy: true },
			{ guard: new StaleDisplayFormGuard(), refresh: (g, oView, s) => g.refresh(oView, s.duplicateProneControls) }
		];
	}

	return ControllerExtension.extend("sap.pc_lite.check.ext.controller.ObjectPageExtension", {

		override: {
			onInit: function () {
				this._aGuards = buildGuardRegistry();
				this._oValueHelpAutoApply = new ValueHelpAutoApply();
				this._oEditableTransitionGate = new EditableTransitionGate();
				this._oSaveIntentTracker = new SaveIntentTracker();
				this._bTickScheduled = false;
				this._fnBoundScheduleTick = this._scheduleTick.bind(this);
			},

			onAfterRendering: function () {
				this._scanTick();
				if (this._bEventsWired) {
					return;
				}
				this._bEventsWired = true;
				const oView = this.getView();
				oView.attachModelContextChange(this._fnBoundScheduleTick);
				// Fiori Elements Input/SmartField fire a UI5 'change' event on
				// suggest/F4 selection, not a native DOM one — see
				// _wireFieldChangeListeners for the real subscription.
				oView.getDomRef().addEventListener("change", this._fnBoundScheduleTick, true);
				const oUiModel = oView.getModel(Constants.UI_MODEL.NAME);
				if (oUiModel) {
					this._oEditableBinding = oUiModel.bindProperty(Constants.UI_MODEL.EDITABLE_PATH);
					this._oEditableBinding.attachChange(this._fnBoundScheduleTick);
				}
			},

			onExit: function () {
				const oView = this.getView();
				oView.detachModelContextChange(this._fnBoundScheduleTick);
				const oViewDom = oView.getDomRef();
				if (oViewDom) {
					oViewDom.removeEventListener("change", this._fnBoundScheduleTick, true);
				}
				if (this._oEditableBinding) {
					this._oEditableBinding.detachChange(this._fnBoundScheduleTick);
					this._oEditableBinding.destroy();
					this._oEditableBinding = null;
				}
				this._oValueHelpAutoApply.destroy();
				(this._aGuards || []).forEach((oEntry) => {
					if (oEntry.destroy && typeof oEntry.guard.destroy === "function") {
						oEntry.guard.destroy();
					}
				});
				this._aGuards = [];
			}
		},

		/** Attaches change listeners to field controls exactly once each. */
		_wireFieldChangeListeners: function (oView, aFieldControls) {
			aFieldControls.forEach((oControl) => {
				if (oControl._pcLiteChangeWired) {
					return;
				}
				oControl._pcLiteChangeWired = true;
				oControl.attachChange(this._fnBoundScheduleTick);
			});
		},

		_scheduleTick: function () {
			if (this._bTickScheduled) {
				return;
			}
			this._bTickScheduled = true;
			Promise.resolve().then(() => {
				this._bTickScheduled = false;
				this._scanTick();
			});
		},

		/**
		 * One tree walk classifying controls by type, plus the shared
		 * editable-transition/save-intent signal — both consumed by multiple
		 * guards instead of each recomputing them independently.
		 */
		_buildScanSnapshot: function (oView) {
			const oSnapshot = { fieldControls: [], buttons: [], duplicateProneControls: [], smartTables: [] };
			oView.findAggregatedObjects(true, (oControl) => {
				if (!oControl.isA) {
					return false;
				}
				if (typeof oControl.attachChange === "function" && Constants.isFieldControl(oControl)) {
					oSnapshot.fieldControls.push(oControl);
				}
				if (oControl.isA("sap.m.Button")) {
					oSnapshot.buttons.push(oControl);
				}
				if (oControl.isA("sap.ui.layout.form.Form") || oControl.isA("sap.ui.comp.smarttable.SmartTable")) {
					oSnapshot.duplicateProneControls.push(oControl);
					if (oControl.isA("sap.ui.comp.smarttable.SmartTable")) {
						oSnapshot.smartTables.push(oControl);
					}
				}
				return false; // classification is a side effect on oSnapshot, not the returned array
			});

			this._oSaveIntentTracker.refresh(oView);
			oSnapshot.editableFell = this._oEditableTransitionGate.consumeFalling(oView);
			oSnapshot.saveWasIntended = this._oSaveIntentTracker.wasSaveIntended();
			if (oSnapshot.editableFell) {
				this._oSaveIntentTracker.reset();
			}
			return oSnapshot;
		},

		/**
		 * Orchestrates the guard registry. Three distinct triggers (DOM capture
		 * 'change', model context change, ui>/editable change) coalesce into one
		 * microtask via _scheduleTick, so _scanTick runs at most once per event cycle.
		 */
		_scanTick: function () {
			const oView = this.getView();
			const oSnapshot = this._buildScanSnapshot(oView);
			this._wireFieldChangeListeners(oView, oSnapshot.fieldControls);

			const oUiModel = oView.getModel(Constants.UI_MODEL.NAME);
			if (oUiModel && oUiModel.getProperty(Constants.UI_MODEL.EDITABLE_PATH)) {
				this._oValueHelpAutoApply.scan(oView, LocationPicker.pickHandler.bind(LocationPicker), this._fnBoundScheduleTick);
			}

			this._aGuards.forEach((oEntry) => oEntry.refresh(oEntry.guard, oView, oSnapshot));
		}
	});
});
