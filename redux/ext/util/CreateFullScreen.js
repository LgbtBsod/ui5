sap.ui.define(["./FioriElementsDom", "./TransientOnce", "./TimeoutTracker"], function (FioriElementsDom, TransientOnce, TimeoutTracker) {
	"use strict";

	/**
	 * Works around a SAPUI5 1.71 bug: after a transient object's first render,
	 * its SmartForm is left parked in the hidden `sap-ui-preserve` area (the
	 * framework never re-parents it back) and stays invisible forever.
	 * `invalidate()`/`rerender()`/`applyChanges()` do not help — only a direct
	 * DOM move of the form into its SubSection does.
	 *
	 * Reacts to an actual DOM mutation on the view (via a scoped
	 * MutationObserver) instead of guessing with fixed retry delays; observation
	 * is bounded by a safety timeout and stops as soon as the move succeeds.
	 */
	class CreateFullScreen {
		constructor() {
			this._oOnce = new TransientOnce();
			this._oTracker = new TimeoutTracker();
			this._oObserver = null;
		}

		applyIfNeeded(oView, fnIsTransient) {
			this._oOnce.runOnce(oView, fnIsTransient, () => this._watchForReparent(oView));
		}

		/** Cancels any pending observation/timers so a destroyed oView is never touched. */
		destroy() {
			this._disconnectObserver();
			this._oTracker.clearAll();
		}

		_watchForReparent(oView) {
			if (CreateFullScreen._reparentSmartFormIfNeeded(oView)) {
				return;
			}
			const oViewDom = oView.getDomRef();
			if (!oViewDom || !window.MutationObserver) {
				this._oTracker.schedule(() => CreateFullScreen._reparentSmartFormIfNeeded(oView), 300);
				return;
			}
			this._oObserver = new MutationObserver(() => {
				if (CreateFullScreen._reparentSmartFormIfNeeded(oView)) {
					this._disconnectObserver();
				}
			});
			this._oObserver.observe(oViewDom, { childList: true, subtree: true });
			// Safety bound in case the form never appears (e.g. section hidden by UI.Hidden).
			this._oTracker.schedule(() => this._disconnectObserver(), 5000);
		}

		_disconnectObserver() {
			if (this._oObserver) {
				this._oObserver.disconnect();
				this._oObserver = null;
			}
		}

		/** @returns {boolean} true once the form is in place (or already was). */
		static _reparentSmartFormIfNeeded(oView) {
			const oForm = CreateFullScreen._findSmartForm(oView);
			if (!oForm) {
				return false;
			}
			const oFormDom = oForm.getDomRef && oForm.getDomRef();
			if (!oFormDom) {
				return false;
			}
			const rect = oFormDom.getBoundingClientRect();
			if (rect.width > 0 && rect.x > -1000) {
				return true;
			}
			// Form id -> SubSection id by suffix — SmartForm/ObjectPageSubSection
			// have no direct API link between them.
			const sSubSectionId = (oFormDom.id || "").replace(/::Form$/, "::SubSection");
			const oSubSection = document.getElementById(sSubSectionId);
			if (!oSubSection) {
				return false;
			}
			const oInnerGrid = oSubSection.querySelector(".sapUiRespGrid") || oSubSection;
			if (oFormDom.parentElement && oFormDom.parentElement.closest("#" + CSS.escape(sSubSectionId))) {
				return true;
			}
			oInnerGrid.appendChild(oFormDom);
			oFormDom.style.visibility = "visible";
			oFormDom.style.left = "";
			oFormDom.style.top = "";
			oFormDom.style.position = "";
			oFormDom.classList.remove("sapUiHidden");
			oFormDom.classList.remove("sapUiForcedHidden");
			if (sap.ui && sap.ui.getCore && typeof sap.ui.getCore().applyChanges === "function") {
				sap.ui.getCore().applyChanges();
			}
			window.dispatchEvent(new Event("resize"));
			return true;
		}

		static _findSmartForm(oView) {
			return FioriElementsDom.findElement("sap.ui.comp.smartform.SmartForm",
				(oElement) => /--GeneralSection::Form$/.test(oElement.getId()), oView);
		}
	}

	return CreateFullScreen;
});
