sap.ui.define(["./Constants"], function (Constants) {
	"use strict";

	const F = Constants.FIELDS;

	/**
	 * [Fix, browser-test pass] LpcKey/ProfKey/Timezone are proxy-keys whose
	 * FieldGroup entry (see annotation/check-root.xml "Bug fix E") points at
	 * the *Text property, not the key - a deliberate choice to avoid
	 * duplicate required-field messages (see that file's own comment).
	 * Common.ValueListWithFixedValues on the *Text property carries a
	 * ValueListParameterOut mapping the picked row's key column back onto
	 * LpcKey/ProfKey/Timezone, which is the standard SAP annotation for
	 * "value help also writes an associated key" - and DOES work for a real
	 * sap.ui.comp.valuehelpdialog.ValueHelpDialog (F4 popup) selection.
	 *
	 * Live-reproduced during browser testing: it does NOT fire for the
	 * sap.m.ComboBox that Common.ValueListWithFixedValues renders instead
	 * (confirmed via direct model inspection - selecting "КПР-2" writes
	 * LpcText correctly but leaves LpcKey null forever). Concretely: the
	 * ComboBox's own items get key===text (both "КПР-2"), and its
	 * selectedKey is bound to LpcText itself, not LpcKey - the Out-parameter
	 * write-back that the ValueHelpDialog path implements was never wired
	 * for this lighter dropdown rendering. Root cause is a real gap between
	 * two SmartField rendering modes, not a one-off typo - so every field
	 * created via this app's own "Bug fix E" fixed-value-list pattern needs
	 * this fix (currently three: КПР/Профессия/Часовой пояс).
	 *
	 * This module closes that gap explicitly: on 'change' of a *Text field
	 * with a registered mapping, it resolves the matching row in the
	 * (small, closed) reference collection and writes the paired key
	 * property directly - the same "controller manually completes what the
	 * framework's fixed-list ComboBox doesn't" approach LocationPicker.js
	 * already uses for Местоположение (which needs its own hierarchy-aware
	 * dialog instead, a bigger lift than a plain lookup here).
	 *
	 * [Fix, browser-test pass #2] Collections are now PRELOADED (see
	 * preload() below, called once per model from wireField) instead of
	 * read lazily inside the 'change' handler. Live-reproduced why the lazy
	 * version was actively harmful, not just slower: sap.ui.model.odata.v2.
	 * ODataModel submits a pending setProperty as its own PATCH on the next
	 * tick unless it lands in an ALREADY-open batch - the SmartField's own
	 * change handling submits LpcText/ProfText/TimezoneText immediately
	 * (tick N), but the old lazy _onChange only called setProperty for the
	 * paired *Key AFTER an async oModel.read() resolved (tick N+k) - by
	 * then the server had already bumped the entity's ETag from the FIRST
	 * PATCH's success, so the *Key-only PATCH (still built against the
	 * stale pre-tick-N ETag) came back 412 Precondition Failed, and neither
	 * PATCH's field ended up actually persisted (confirmed via direct
	 * server-side inspection: LpcKey/ProfKey/LpcText/ProfText all stayed
	 * empty after a "successful"-looking selection). Preloading means the
	 * lookup is a synchronous array scan by the time 'change' fires for any
	 * realistic user interaction, so the *Key write happens in the SAME
	 * tick/batch as the *Text write - one PATCH, one ETag, no race.
	 */
	const MAPPINGS = {
		[F.LPC_TEXT]: { keyField: F.LPC_KEY, collectionPath: "/" + Constants.ENTITY_SETS.PK_LEVELS, keyProp: "PkLevel", textProp: "PkLevelText" },
		[F.PROF_TEXT]: { keyField: F.PROF_KEY, collectionPath: "/" + Constants.ENTITY_SETS.PROFESSIONS, keyProp: "ProfessionCode", textProp: "ProfessionText" },
		[F.TIMEZONE_TEXT]: { keyField: F.TIMEZONE, collectionPath: "/" + Constants.ENTITY_SETS.TIME_ZONES, keyProp: "TimeZoneCode", textProp: "TimeZoneText" }
	};

	// collectionPath -> Array<row> once resolved - each of the three
	// reference lists is small (<=9 rows) and effectively static for the
	// process lifetime, so one read per collection per page load is enough;
	// no invalidation needed.
	const _oResolvedRows = {};
	// collectionPath -> Promise, only consulted as a fallback for the rare
	// case a 'change' fires before preload's read has resolved yet.
	const _oPendingReads = {};
	// WeakSet-by-model (WeakMap<model, true>) - preload() is safe to call
	// once per wireField invocation; only the FIRST call per model actually
	// issues the three reads.
	const _oPreloadedModels = new WeakMap();

	function startLoad(oModel, sCollectionPath) {
		if (_oResolvedRows[sCollectionPath] || _oPendingReads[sCollectionPath]) {
			return;
		}
		_oPendingReads[sCollectionPath] = new Promise((resolve) => {
			oModel.read(sCollectionPath, {
				success: (oData) => {
					const aRows = (oData && oData.results) || [];
					_oResolvedRows[sCollectionPath] = aRows;
					delete _oPendingReads[sCollectionPath];
					resolve(aRows);
				},
				error: () => {
					delete _oPendingReads[sCollectionPath];
					resolve([]);
				}
			});
		});
	}

	class FixedListKeySync {
		/** Kicks off all three reference-collection reads once per model -
		 * call as early as possible (see ObjectPageExtension._wireFieldChangeListeners)
		 * so they're resolved well before a real user picks a value. */
		static preload(oModel) {
			if (!oModel || _oPreloadedModels.has(oModel)) {
				return;
			}
			_oPreloadedModels.set(oModel, true);
			Object.keys(MAPPINGS).forEach((sTextPath) => startLoad(oModel, MAPPINGS[sTextPath].collectionPath));
		}

		/**
		 * Wires oControl once if its "value" binding path matches one of
		 * MAPPINGS - safe to call for every field control on the page
		 * (see ObjectPageExtension._wireFieldChangeListeners), controls
		 * with no matching path are a no-op.
		 */
		static wireField(oControl) {
			if (oControl._pcLiteKeySyncWired || typeof oControl.getBindingPath !== "function") {
				return;
			}
			const sPath = oControl.getBindingPath("value");
			const oMapping = sPath && MAPPINGS[sPath];
			if (!oMapping) {
				return;
			}
			oControl._pcLiteKeySyncWired = true;
			const oModel = oControl.getModel();
			if (oModel) {
				FixedListKeySync.preload(oModel);
			}
			oControl.attachChange(() => FixedListKeySync._onChange(oControl, sPath, oMapping));
		}

		static _writeKey(oContext, sPath, oMapping, sText, aRows) {
			// Re-read the context's current text at resolution time, not the
			// value captured at attach-time - guards against a fast second
			// selection racing the first (only matters for the async
			// fallback path below; the synchronous path can't race with
			// itself since nothing else runs between the read and the write).
			if (oContext.getProperty(sPath) !== sText) {
				return;
			}
			const oMatch = aRows.find((oRow) => oRow[oMapping.textProp] === sText);
			oContext.getModel().setProperty(oContext.getPath() + "/" + oMapping.keyField, oMatch ? oMatch[oMapping.keyProp] : null);
		}

		static _onChange(oControl, sPath, oMapping) {
			const oContext = oControl.getBindingContext();
			if (!oContext) {
				return;
			}
			const sText = oContext.getProperty(sPath);
			const aCached = _oResolvedRows[oMapping.collectionPath];
			if (aCached) {
				// Common path: preload already resolved (it starts as soon as
				// the page's fields are first scanned, well before any real
				// user picks a value) - write happens synchronously in the
				// SAME tick as this change event, landing in the same PATCH/
				// batch as the framework's own *Text write.
				FixedListKeySync._writeKey(oContext, sPath, oMapping, sText, aCached);
				return;
			}
			// Rare fallback: change fired before preload's read resolved
			// (e.g. instant first interaction). Still correct, just carries
			// the same theoretical race the old always-lazy version had.
			const oModel = oContext.getModel();
			startLoad(oModel, oMapping.collectionPath);
			(_oPendingReads[oMapping.collectionPath] || Promise.resolve(_oResolvedRows[oMapping.collectionPath] || []))
				.then((aRows) => FixedListKeySync._writeKey(oContext, sPath, oMapping, sText, aRows));
		}
	}

	return FixedListKeySync;
});
