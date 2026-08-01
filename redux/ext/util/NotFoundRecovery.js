sap.ui.define(["./FioriElementsDom", "./DomMutationBus", "./AppNavigation"], function (FioriElementsDom, DomMutationBus, AppNavigation) {
        "use strict";

        // sap.suite.ui.generic.template хранит в истории браузера hash транзитного
        // (ещё не сохранённого) объекта; после Save он заменяется постоянным ключом,
        // но старая запись остаётся в history. Кнопка "Назад" может увести на неё и
        // упереться в стандартный MessagePage "не найдено" — наблюдаем за появлением
        // этого экрана (по типу контрола, не по тексту) и уводим пользователя на список.
        class NotFoundRecovery {
                constructor() {
                        this._fnUnsubscribe = null;
                }

                start() {
                        if (this._fnUnsubscribe) {
                                return;
                        }
                        this._fnUnsubscribe = DomMutationBus.subscribe((aMutations) => this._check(aMutations));
                }

                stop() {
                        if (this._fnUnsubscribe) {
                                this._fnUnsubscribe();
                                this._fnUnsubscribe = null;
                        }
                }

                // Полный обход sap.ui.core.Element.registry (см. FioriElementsDom.findElement
                // без oScope) нужен только когда мутация реально добавила элемент-узел —
                // MessagePage не может появиться иначе; на мутациях с пустыми/только
                // текстовыми addedNodes обход пропускается.
                _check(aMutations) {
                        const bMayHaveAddedElement = (aMutations || []).some((oMutation) =>
                                Array.prototype.some.call(oMutation.addedNodes, (oNode) => oNode.nodeType === 1)
                        );
                        if (!bMayHaveAddedElement) {
                                return;
                        }
                        // Намеренно без oScope: MessagePage подменяет содержимое всей
                        // страницы (в т.ч. сам view, который мы бы искали) — сузить поиск
                        // до конкретного view здесь невозможно по построению, это не
                        // недосмотр, а единственный вариант для этого сценария.
                        const oMessagePage = FioriElementsDom.findElement("sap.m.MessagePage", () => true);
                        if (oMessagePage && oMessagePage.getDomRef()) {
                                AppNavigation.navigateToRoot();
                        }
                }
        }

        return NotFoundRecovery;
});
