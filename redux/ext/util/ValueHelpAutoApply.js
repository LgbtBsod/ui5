sap.ui.define(["./DomMutationBus", "./OnceRegistry"], function (DomMutationBus, OnceRegistry) {
        "use strict";

        const _oAutoAppliedDialogs = new OnceRegistry();

        // WeakMap вместо monkey-patching oElement._fnAutoSearchReset — не
        // загрязняет пространство имён диалога.
        const _mResetCallbacks = new WeakMap();

// [UAT-фикс] FilterBar внутри ValueHelpDialog заполняет метаданные
// асинхронно, ПОСЛЕ появления самого диалога в DOM — вызов search() до
// этого момента логировал "SmartFilterBar.search: called before the
// SmartFilterBar is initialized" (найдено в тестировании, безвредно для
// пользователя, но грязный лог на штатном пути). getFilterBarViewMetadata()
// и событие initialise — штатный, документированный признак готовности
// sap.ui.comp.filterbar.FilterBar, а не самодельный таймер/ретрай.
function pressGo(oValueHelp) {
                const oFilterBar = oValueHelp.getFilterBar && oValueHelp.getFilterBar();
                if (!oFilterBar || !oFilterBar.search) {
                        return;
                }
                if (oFilterBar.getFilterBarViewMetadata && oFilterBar.getFilterBarViewMetadata()) {
                        oFilterBar.search();
                } else if (typeof oFilterBar.attachInitialise === "function") {
                        oFilterBar.attachInitialise(function onInitialise() {
                                oFilterBar.detachInitialise(onInitialise);
                                oFilterBar.search();
                        });
                }
        }

        function attachResetOnce(oElement, fnReset) {
                const fnOldReset = _mResetCallbacks.get(oElement);
                if (fnOldReset) {
                        oElement.detachOk(fnOldReset);
                        oElement.detachCancel(fnOldReset);
                        oElement.detachAfterClose(fnOldReset);
                }
                _mResetCallbacks.set(oElement, fnReset);
                oElement.attachOk(fnReset);
                oElement.attachCancel(fnReset);
                oElement.attachAfterClose(fnReset);
        }

        function detachReset(oElement) {
                const fnReset = _mResetCallbacks.get(oElement);
                if (fnReset) {
                        oElement.detachOk(fnReset);
                        oElement.detachCancel(fnReset);
                        oElement.detachAfterClose(fnReset);
                        _mResetCallbacks.delete(oElement);
                }
        }

        // Реагирует на появление ValueHelpDialog через общую DomMutationBus, без
        // периодического опроса registry. scan() идемпотентен: повторные вызовы — no-op.
        class ValueHelpAutoApply {
                constructor() {
                        this._fnUnsubscribe = null;
                }

                // fnAfterFieldChange — опциональный явный callback (например
                // ObjectPageExtension._fnBoundScheduleTick), передаваемый дальше в
                // fnSpecialSetup (см. LocationPicker.replace) вместо того, чтобы
                // спецобработчик сам придумывал способ уведомить контроллер об
                // изменении поля.
                scan(oView, fnPickHandler, fnAfterFieldChange) {
                        if (this._fnUnsubscribe) {
                                return;
                        }
                        this._fnUnsubscribe = DomMutationBus.subscribe(
                                (aMutations) => this._onMutations(aMutations, oView, fnPickHandler, fnAfterFieldChange)
                        );
                }

                _onMutations(aMutations, oView, fnPickHandler, fnAfterFieldChange) {
                        aMutations.forEach((oMutation) => {
                                oMutation.addedNodes.forEach((oNode) => this._onNodeAdded(oNode, oView, fnPickHandler, fnAfterFieldChange));
                        });
                }

                _onNodeAdded(oNode, oView, fnPickHandler, fnAfterFieldChange) {
                        if (oNode.nodeType !== 1 || !oNode.id) {
                                return;
                        }
                        const oElement = sap.ui.core.Element.registry.get(oNode.id);
                        if (!oElement || !oElement.isA || !oElement.isA("sap.ui.comp.valuehelpdialog.ValueHelpDialog")) {
                                return;
                        }
                        if (_oAutoAppliedDialogs.isMarked(oElement) || oElement.getId().indexOf(oView.getId()) !== 0) {
                                return;
                        }
                        const oFilterBar = oElement.getFilterBar && oElement.getFilterBar();
                        if (!oFilterBar) {
                                return;
                        }
                        _oAutoAppliedDialogs.mark(oElement);

                        const fnSpecialSetup = fnPickHandler(oFilterBar.getEntitySet());
                        if (fnSpecialSetup) {
                                oElement.addStyleClass("pcFlashGuardHidden");
                                fnSpecialSetup(oElement, oView, fnAfterFieldChange);
                        } else {
                                pressGo(oElement);
                                attachResetOnce(oElement, () => {
                                        _oAutoAppliedDialogs.unmark(oElement);
                                        detachReset(oElement);
                                });
                        }
                }

                destroy() {
                        if (this._fnUnsubscribe) {
                                this._fnUnsubscribe();
                                this._fnUnsubscribe = null;
                        }
                }
        }

        return ValueHelpAutoApply;
});
