sap.ui.define([], function () {
        "use strict";

        class DialogLifecycle {
                /**
                 * @param {sap.m.Dialog|null} oDialog
                 * @param {sap.ui.model.Model[]} aModels
                 */
                static destroyWithModels(oDialog, aModels) {
                        if (oDialog && !oDialog.bIsDestroyed) {
                                oDialog.destroy();
                        }

                        (aModels || []).forEach((oModel) => {
                                if (!oModel || oModel.bIsDestroyed) {
                                        return;
                                }

                                const aBindings = oModel.getBindings ? oModel.getBindings() : [];
                                const iBindingCount = aBindings ? aBindings.length : 0;

                                // Модель может быть привязана к другому контролу (например view) —
                                // destroy() тогда оставил бы висячие ссылки. Не трогаем, GC подберёт.
                                if (iBindingCount > 0) {
                                        return;
                                }

                                oModel.destroy();
                        });
                }
        }

        return DialogLifecycle;
});
