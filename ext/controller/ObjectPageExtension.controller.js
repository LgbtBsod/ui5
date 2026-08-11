sap.ui.define([
	"sap/ui/core/mvc/ControllerExtension",
	"../util/Constants",
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
	"../util/StaleDisplayFormGuard",
	"../util/EditableTransitionGate",
	"../util/SaveIntentTracker"
], function (
	ControllerExtension, Constants, ODataContextUtils,
	KpiSync, DateTimeAutofill, ValueHelpAutoApply, LocationPicker, SubTableCrud,
	CreateFullScreen, KeepEditMode, FieldStateGate, PernrRule, CheckCodeCoverage,
	RequiredFieldsRule, IntegrationEditGuard, DiscardTransientOnClose, StaleDisplayFormGuard,
	EditableTransitionGate, SaveIntentTracker
) {
	"use strict";

	/**
	 * @returns {object} sub-table CRUD config for one Object Page section.
	 * [Fix, inline-create pass] Trimmed to just navProperty (which
	 * composition SubTableCrud.addRow() deep-inserts into) now that the
	 * row-add UI is the table itself, not a separate dialog - typeLabel/
	 * dialogTitle existed only to feed dialog-specific chrome (a title bar,
	 * a "Выберите <label>" validation toast) that no longer exists. The
	 * new row's field labels/required-ness/F4 all come straight from
	 * Common.Label/Common.Required/Common.ValueList on CheckItem/Barrier's
	 * own annotations, same single source of truth as every other row.
	 */
	function buildSectionConfig(sNavProperty) {
		return { navProperty: sNavProperty };
	}

	const PAGE_CONFIG = {
		sections: {
			[Constants.FACET_IDS.CHECK_ROWS_SECTION]: buildSectionConfig(Constants.NAV_PROPERTIES.CHECK_ROWS),
			[Constants.FACET_IDS.BARRIERS_SECTION]: buildSectionConfig(Constants.NAV_PROPERTIES.BARRIERS)
		}
	};

	/**
	 * Guard registry: each entry is {guard, refresh(guard, oView, oSnapshot), destroy?}.
	 * Adding a guard means adding one entry here — not touching onInit/onExit/_scanTick.
	 *
	 * [Fix, standard-extensions pass] Two guards removed from this registry:
	 * - SubTableCrud used to be scanned/ticked here purely to inject its own
	 *   toolbar buttons at runtime (the anti-pattern SAP's own extensibility
	 *   docs warn against). It's no longer a "guard" at all - it's now a
	 *   plain reusable dialog/OData-logic helper (this._oSubTableCrud, see
	 *   onInit) invoked directly by the new onAddCheckRow/onAddBarrier
	 *   Sections.Actions press handlers below, so it never appears in this
	 *   registry.
	 * - DeleteConfirmGuard removed - live-tested this session that the
	 *   Object Page's native single-record Delete already shows a full
	 *   confirmation dialog with ZERO custom JS active (standard Fiori
	 *   Elements draft behavior). That guard's own comment claiming
	 *   otherwise was written for/against a non-draft template; re-enabling
	 *   it would be redundant at best and, since it replaces the framework's
	 *   own delete handling with a raw oModel.remove() call bypassing the
	 *   correctly-integrated draft-delete flow, actively risky at worst.
	 */
	function buildGuardRegistry() {
		const oCodeCoverage = new CheckCodeCoverage();
		return [
			{ guard: new DateTimeAutofill(), refresh: (g, oView) => g.applyIfNeeded(oView, ODataContextUtils.isUnsavedNewDraft) },
			{ guard: KpiSync, refresh: (g, oView) => g.sync(oView, ODataContextUtils.isUnsavedNewDraft) },
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
				// [Fix, standard-extensions pass] Plain reusable dialog/OData
				// helper for the Sections.Actions "Создать" buttons below -
				// not a ticked guard, just instantiated once and called
				// directly from onAddCheckRow/onAddBarrier.
				this._oSubTableCrud = new SubTableCrud();
				this._bTickScheduled = false;
				this._fnBoundScheduleTick = this._scheduleTick.bind(this);
			},

			onAfterRendering: function () {
				this._scanTick();
				this._wireSectionActionButtons();
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

		/**
		 * [Fix, standard-extensions pass] Sections.Actions press handlers -
		 * registered declaratively in manifest.json
		 * (sap.ui5.extends.extensions.sap.ui.controllerExtensions ->
		 * "sap.suite.ui.generic.template.ObjectPage.view.Details" ->
		 * sap.ui.generic.app.CheckRoots.Sections.CheckRowsSection/
		 * BarriersSection.Actions.*.press = "onAddCheckRow"/"onAddBarrier",
		 * bare method names resolved directly on this controller extension
		 * instance). This is the SAP-documented, framework-native way to add
		 * a toolbar action button to an Object Page section - confirmed via
		 * two independent research passes to be the sanctioned replacement
		 * for the old SubTableCrud.js's now-removed approach of destroying
		 * the framework's own (permanently-hidden-anyway) native addEntry
		 * button and injecting a raw sap.m.Button in its place. this.extensionAPI
		 * is the real sap.suite.ui.generic.template.ObjectPage.extensionAPI.
		 * ExtensionAPI instance (confirmed via direct 1.71.84 source fetch),
		 * already available on any registered controller extension - no
		 * getExtensionAPIPromise() indirection needed here since this
		 * controller IS the one wired to the Object Page view directly (that
		 * indirection is only for controls embedded via a separate
		 * viewExtensions fragment, which this app doesn't use for this).
		 */
		/**
		 * [Fix, standard-extensions pass] The declarative manifest.json
		 * "press": "onAddCheckRow" string (sap.ui.generic.app.CheckRoots.
		 * Sections.<facetId>.Actions) never fires: confirmed live (button
		 * renders with correct id/text, but sap.ui.core.mvc.
		 * EventHandlerResolver -> sap/base/util/resolveReference resolves a
		 * bare handler name via a plain `"onAddCheckRow" in oController`
		 * check against the UNEXTENDED base template controller - it has no
		 * knowledge of sap.ui.controllerExtensions' internal registry, where
		 * this method actually lives two levels deep
		 * (oController._sapui_Extensions[ourNamespace].extension.
		 * onAddCheckRow). This is a framework resolution gap for this
		 * specific breakout-action-on-table-facet mechanism, not a manifest
		 * mistake - verified by fetching EventHandlerResolver-dbg.js and
		 * resolveReference-dbg.js directly from the real 1.71.84 CDN source.
		 * The button itself IS the standard, documented extension point
		 * (sap.ui.generic.app.Sections.Actions); this just wires its press
		 * event the same way any other standard control extension point
		 * would be wired from onAfterRendering, via plain attachPress() -
		 * no DOM manipulation, no control replacement.
		 */
		_wireSectionActionButtons: function () {
			const oView = this.getView();
			[
				["addCheckRowAction", () => this.onAddCheckRow()],
				["addBarrierAction", () => this.onAddBarrier()]
			].forEach(([sId, fnHandler]) => {
				const oButton = oView.byId(sId);
				if (oButton && !oButton._pcLiteActionPressWired) {
					oButton._pcLiteActionPressWired = true;
					oButton.attachPress(fnHandler);
					// [Fix, use-case pass] The declarative manifest.json Action
					// config has no "visible"/"enabled" property tied to edit
					// mode (confirmed: no such option is documented for
					// Sections.Actions, unlike the framework's OWN native
					// Create/Delete buttons - see CRUDActionButtons.fragment.xml's
					// visible="{= ${parameter>/isDraftEnabled} ? '{ui>/editable}'
					// ...}" for the same pattern on Delete). Without this,
					// "Добавить" stayed clickable in DISPLAY mode too - live-
					// tested: it deep-inserted the new CheckItem/Barrier directly
					// against the ACTIVE entity (DraftUUID=zero-guid,
					// IsActiveEntity=true) with no draft involved at all,
					// bypassing the whole draft/edit transaction - explaining
					// why a row added this way looked "invisible until save"
					// (the SmartTable's active-scoped binding was never
					// refetched to include it; only a subsequent real edit+save
					// cycle synced the client). Bound here, alongside the press
					// handler, for the same structural reason: standard
					// Control.bindProperty against the same "ui" model/
					// "/editable" path every other edit-mode-gated control in
					// this app already uses (see Constants.UI_MODEL) - not a
					// new mechanism, just applied to a button the declarative
					// path can't reach.
					oButton.bindProperty("visible", {
						model: Constants.UI_MODEL.NAME,
						path: Constants.UI_MODEL.EDITABLE_PATH
					});
				}
			});
		},

		onAddCheckRow: function () {
			// [Fix, standard-extensions pass] `this.extensionAPI` is undefined
			// here - confirmed live via runtime inspection: when a controller
			// extension method is invoked through a manually-wired press
			// handler (see _wireSectionActionButtons), `this` is the extension
			// registry's inner instance, which does NOT carry its own
			// `extensionAPI` (only the outer/base template controller does,
			// and `this.base` doesn't either). `this.getView()` DOES resolve
			// correctly on this instance, and the view's controller is that
			// same outer/base controller - so routing through the view is the
			// reliable path to the real ExtensionAPI in this call context.
			const oExtensionAPI = this.getView().getController().extensionAPI;
			this._oSubTableCrud.addRow(
				this.getView(),
				PAGE_CONFIG.sections[Constants.FACET_IDS.CHECK_ROWS_SECTION],
				// [Fix, use-case pass] rebind() needs the SmartTable's own local id,
				// not the facet's - see Constants.ID_SUFFIXES.SMART_TABLE. The bare
				// facet id resolved to undefined via byId(), so rebindTable() never
				// fired and an added row stayed invisible until the outer Save.
				() => oExtensionAPI.rebind(Constants.FACET_IDS.CHECK_ROWS_SECTION + Constants.ID_SUFFIXES.SMART_TABLE)
			);
		},

		onAddBarrier: function () {
			const oExtensionAPI = this.getView().getController().extensionAPI;
			this._oSubTableCrud.addRow(
				this.getView(),
				PAGE_CONFIG.sections[Constants.FACET_IDS.BARRIERS_SECTION],
				() => oExtensionAPI.rebind(Constants.FACET_IDS.BARRIERS_SECTION + Constants.ID_SUFFIXES.SMART_TABLE)
			);
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
