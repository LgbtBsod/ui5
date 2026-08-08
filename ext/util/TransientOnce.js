sap.ui.define(["./OnceRegistry"], function (OnceRegistry) {
        "use strict";

        // Обёртка над OnceRegistry.onceByKey с доменным API: runOnce принимает
        // oView/fnIsTransient и сам извлекает binding-context/path.
        class TransientOnce {
                constructor() {
                        this._oRegistry = new OnceRegistry();
                }

                runOnce(oView, fnIsTransient, fnAction) {
                        const oContext = oView.getBindingContext();
                        if (!oContext || !fnIsTransient(oContext)) {
                                return;
                        }
                        const sPath = oContext.getPath();
                        this._oRegistry.onceByKey(sPath, () => fnAction(oContext, sPath));
                }
        }

        return TransientOnce;
});
