sap.ui.define([
	"./Constants",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"sap/ui/model/FilterType"
], function (Constants, Filter, FilterOperator, FilterType) {
	"use strict";

	const F = Constants.FIELDS;

	// [Fix] Live-confirmed race: right after the FilterBar's own first
	// search() call, the ValueHelpDialog's table CONTROL already exists but
	// its "rows"/"items" BINDING doesn't yet - the dialog binds the table
	// asynchronously as part of that same search cycle, so a filter() call
	// this early silently no-ops against an undefined binding. Also
	// live-confirmed: sap.ui.table.Table in this UI5 version (1.71.84) has
	// NO "rowsUpdated" event at all (checked getMetadata().getAllEvents() -
	// not in the list), so there is no reliable public event to wait on
	// either. A short bounded retry is the pragmatic fix: cheap, safe (hard
	// cap so a permanently-missing binding can't loop forever), and doesn't
	// depend on guessing which framework-internal signal fires when.
	const RETRY_DELAY_MS = 50;
	const MAX_RETRIES = 20; // 1s budget - the dialog's own first fetch is fast (in-memory mock)

	/**
	 * [Fix, frontend-bug-fix pass] annotation/check-root.xml maps the local
	 * `Date` field to Persons' `ActiveFrom`/`ActiveTo` via two
	 * `Common.ValueListParameterFilterOnly` records - the standard SAP
	 * convention for "as-of" value-help filtering. Live-confirmed this does
	 * NOT translate into a silent range constraint in this SAPUI5 1.71.84
	 * classic `sap.ui.comp.valuehelpdialog.ValueHelpDialog`: the two mapped
	 * properties instead render as ordinary, EMPTY, user-facing "Активен
	 * с"/"Активен по" advanced-search fields - no automatic filter is ever
	 * sent, so an inactive Person (ActiveTo in the past) stays selectable
	 * for a check dated well after they left.
	 *
	 * Same fix shape as the Location as-of versioning feature
	 * (LocationPicker._effectiveDateFilter), just applied to a *standard*
	 * ValueHelpDialog's table instead of a fully custom picker: an
	 * Application-type `sap.ui.model.Filter` on the table's own
	 * ODataListBinding. Application-type filters are additive and persist
	 * independently of whatever Control-type filter the FilterBar's own
	 * search() sends - live-confirmed the table auto-refetches and
	 * correctly excludes/includes rows the moment `.filter()` is called,
	 * with no further wiring needed.
	 */
	class PersonActiveFilter {
		/**
		 * No-op for every entity set except Persons, and every ValueHelpDialog
		 * mode without a real check Date to anchor on (filterBar mode - no
		 * single check to ask "active as of when"; a brand-new create draft
		 * whose Date isn't filled in yet). Otherwise delegates to
		 * _applyWithRetry, which handles the table-binding-not-ready-yet race
		 * (see the file-level comment above).
		 * @param {string} sEntitySet
		 * @param {sap.ui.comp.valuehelpdialog.ValueHelpDialog} oValueHelp
		 * @param {sap.ui.core.mvc.View} oView
		 */
		static applyIfApplicable(sEntitySet, oValueHelp, oView) {
			if (sEntitySet !== Constants.ENTITY_SETS.PERSONS) {
				return;
			}
			const oHeaderContext = oView.getBindingContext();
			if (!oHeaderContext) {
				return;
			}
			const oDate = oHeaderContext.getProperty(F.BASIC_DATE);
			if (!oDate) {
				return;
			}
			PersonActiveFilter._applyWithRetry(oValueHelp, oDate, MAX_RETRIES);
		}

		/**
		 * @param {sap.ui.comp.valuehelpdialog.ValueHelpDialog} oValueHelp
		 * @param {Date} oDate
		 * @param {number} iAttemptsLeft
		 */
		static _applyWithRetry(oValueHelp, oDate, iAttemptsLeft) {
			const oTable = oValueHelp.getTable && oValueHelp.getTable();
			const oBinding = oTable && oTable.getBinding
				&& (oTable.getBinding("rows") || oTable.getBinding("items"));
			if (!oBinding) {
				// Dialog was closed before its table ever bound (user
				// closed instantly), or we've exhausted the retry budget -
				// either way, stop rather than retry forever.
				if (iAttemptsLeft > 0 && !oValueHelp.bIsDestroyed) {
					setTimeout(() => PersonActiveFilter._applyWithRetry(oValueHelp, oDate, iAttemptsLeft - 1), RETRY_DELAY_MS);
				}
				return;
			}
			oBinding.filter([
				new Filter(F.ACTIVE_FROM, FilterOperator.LE, oDate),
				// ActiveTo is null/undefined for a still-active person (see
				// serve_config.py's _build_persons) - "still active" and
				// "active until >= the check's date" are both acceptable.
				new Filter({
					filters: [
						new Filter(F.ACTIVE_TO, FilterOperator.EQ, null),
						new Filter(F.ACTIVE_TO, FilterOperator.GE, oDate)
					],
					and: false
				})
			], FilterType.Application);
		}
	}

	return PersonActiveFilter;
});
