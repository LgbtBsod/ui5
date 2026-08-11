sap.ui.define([
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"./Constants",
	"./ODataUtils"
], function (Filter, FilterOperator, Constants, ODataUtils) {
	"use strict";

	const F = Constants.FIELDS;

	// [Fix, standard-extensions pass] This class used to ALSO own a
	// toolbar-button-injection step (find the SmartTable, destroy the
	// framework's native addEntry/deleteEntry controls, imperatively
	// oToolbar.addContent(new Button(...))) - exactly the anti-pattern SAP's
	// own Fiori Elements extensibility docs warn against ("use app
	// extensions with caution... use only the extensionAPI"), confirmed by
	// two independent research passes. That machinery is gone: the
	// composition-child SmartTable's own native deleteEntry button was
	// already confirmed live-working out of the box (standard delete
	// confirmation dialog, zero custom JS - see the DeleteConfirmGuard
	// audit), so no custom delete button is needed either. The native
	// addEntry button is confirmed unconditionally hidden in this UI5
	// 1.71.84 build (hardcoded visible:false, no binding at all) - the
	// standard, documented replacement is a declarative Action button
	// (manifest.json's sap.ui.controllerExtensions ->
	// sap.ui.generic.app.CheckRoots.Sections.<facetId>.Actions, see
	// ObjectPageExtension.controller.js's onAddCheckRow/onAddBarrier),
	// NOT a runtime-injected Button replacing a destroyed native one. This
	// class now owns ONLY the OData create logic that Action calls into -
	// the old OnceRegistry-based "have I already scanned this SmartTable"
	// dedup bookkeeping is gone along with the scan itself.
	//
	// [Fix, use-case pass] The row-add UI used to be a modal Dialog built
	// from plain sap.m.Input controls + a hand-rolled TypePicker/
	// LocalItemSuggest pair (custom JSON model, custom Contains-filter,
	// custom Dialog+Table) for F4/type-ahead on the Type/Result fields - a
	// SEPARATE search implementation from the one users see inline-editing
	// an EXISTING row in the same table. Fixed by switching the dialog's
	// plain Inputs for real SmartFields bound to the same annotations -
	// one search implementation, not two. TypePicker.js and
	// LocalItemSuggest.js are no longer used anywhere and were removed.
	//
	// [Fix, inline-create pass] The Dialog itself is gone now too. It was a
	// SmartForm wrapping the exact same SmartFields the table's own inline
	// row-edit already renders for every other row - a second UI shell
	// around identical controls, for no functional reason once the field
	// content had already been unified. addRow() now does exactly two
	// things: createEntry() the transient child row (unchanged deep-insert
	// against the nav property) and hand back control to the caller, which
	// rebinds the SmartTable so the new row appears immediately as a normal,
	// already-editable row - same SmartField cells, same F4/dropdown, as
	// every pre-existing row. The user fills it in right there in the
	// table, exactly like editing any other row.
	//
	// This also means the class no longer calls oModel.submitChanges()
	// itself. It used to (with a dedicated deferred group, to stop that
	// call from also sweeping up and racing unrelated pending header-field
	// changes against the framework's own autosave - see git history for
	// the live-reproduced 412/silent-navigate-away bug that caused). The
	// deeper fix is not to submit at all: a freshly createEntry()'d row is
	// just another pending change sitting in the model's default group,
	// exactly like typing into any other field on this page - and typing
	// into any OTHER field doesn't get its own manual submitChanges() call
	// either. It rides the framework's own periodic draft-autosave / the
	// user's eventual Save, the same as everything else on this page.
	// Confirmed live that this is genuinely the existing behavior to match,
	// not a regression: editing an EXISTING row's Result and watching the
	// header's "Успешных проверок: N" counter shows it does NOT live-update
	// on its own either - it only catches up on the next real read (Save,
	// navigation, or the framework's own autosave), same lag a new row now
	// has. Special-casing the create flow to eagerly re-read and refresh
	// that counter (the old _refreshHeaderKpi/_readHeaderKpi pair) was
	// inconsistent with how every other edit on the page already behaves -
	// removed along with the manual submit.
	//
	// Common.Required=true is already declared on CheckItem/Barrier's own
	// Text property (see annotation/check-item.xml, annotation/barrier.xml)
	// - RequiredFieldsRule.js's existing Save-blocking gate already stops an
	// incomplete new row (Type not picked) from being saved, so this class
	// doesn't need its own "did you pick a type" validation either.
	class SubTableCrud {

		/**
		 * Adds a new row directly into a composition-child table (CheckItems/
		 * Barriers) - inline, no dialog. Creates the transient entity via
		 * deep-insert (createEntry against the nav property) with a sensible
		 * default Result, then hands control back to the caller so it can
		 * rebind the SmartTable and make the row visible for editing. This is
		 * the body of the declarative Sections.Actions press handler (see
		 * ObjectPageExtension.controller.js's onAddCheckRow/onAddBarrier) -
		 * the standard replacement for the framework's own native addEntry
		 * button, which is confirmed permanently hidden for a composition-
		 * child table on the Object Page in this UI5 version.
		 *
		 * @param {sap.ui.core.mvc.View} oView
		 * @param {object} oConfig one entry from ObjectPageExtension's
		 *   PAGE_CONFIG.sections ({navProperty})
		 * @param {function(): void} [fnAfterCreate] called right after the
		 *   transient row is created - the caller wires this to
		 *   extensionAPI.rebind(sFacetId) (the documented, targeted refresh
		 *   call), so this class stays decoupled from the ExtensionAPI/
		 *   facet-id addressing itself.
		 */
		addRow(oView, oConfig, fnAfterCreate) {
			const oHeaderContext = oView.getBindingContext();
			if (!oHeaderContext) {
				return;
			}
			const oModel = oView.getModel();
			const oRowContext = oModel.createEntry(oConfig.navProperty, {
				context: oHeaderContext,
				properties: { [F.RESULT]: Constants.RESULT_CODES.SATISFACTORY }
			});
			// Единственное оставшееся справочное чтение - подставить
			// отображаемый текст дефолтного результата (код уже проставлен
			// выше через properties). Без этого поле показывало бы пустой
			// текст, хотя скрытый код результата уже "Удовлетворительно" -
			// Common.Text не резолвится клиентом автоматически из голого кода.
			this._seedDefaultResultText(oModel, oRowContext);
			if (typeof fnAfterCreate === "function") {
				fnAfterCreate();
			}
		}

		_seedDefaultResultText(oModel, oRowContext) {
			ODataUtils.readEntitySet(oModel, `/${Constants.ENTITY_SETS.CHECK_RESULTS}`, {
				filters: [new Filter(F.RESULT_CODE, FilterOperator.EQ, Constants.RESULT_CODES.SATISFACTORY)],
				urlParameters: { "$select": F.RESULT_TEXT_REF }
			}).then((aResults) => {
				const sPath = oRowContext.getPath();
				// Гонка: пользователь мог уже сам поменять результат, пока
				// справочник грузился - тогда чужой дефолт не подставляем.
				if (aResults.length && oModel.getProperty(`${sPath}/${F.RESULT}`) === Constants.RESULT_CODES.SATISFACTORY) {
					oModel.setProperty(`${sPath}/${F.RESULT_TEXT}`, aResults[0][F.RESULT_TEXT_REF]);
				}
			});
		}
	}

	return SubTableCrud;
});
