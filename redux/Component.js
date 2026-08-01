sap.ui.define([
        "sap/suite/ui/generic/template/lib/AppComponent",
        "./ext/util/LiteCreateMode",
        "./ext/util/NotFoundRecovery"
], (AppComponent, LiteCreateMode, NotFoundRecovery) => {
        "use strict";

        // start() вызывается здесь, на уровне фабрики модуля, а НЕ из
        // переопределённого init(): sap.suite.ui.generic.template.lib.AppComponent
        // не вызывает init() дочернего класса, поэтому этот код там не выполнился бы
        // (подтверждено вживую — ?lite перестаёт активироваться). Оба класса не зависят
        // от `this`/состояния Component, только от window.location и registry элементов,
        // так что вызов здесь эквивалентен init(), но реально работает. Не переносить
        // в init() без regression-теста на ?lite.
        //
        // [Аудит: Component.js module-singleton] Следствие такого размещения —
        // oLiteCreateMode/oNotFoundRecovery создаются РОВНО ОДИН РАЗ на модуль
        // (sap.ui.define кэширует фабрику), а не по одному на экземпляр Component;
        // exit() любого экземпляра остановит оба сервиса для всех. Это осознанно
        // безопасно для фактической модели использования этого приложения:
        // index.html вызывает sap.ui.core.Component.create() ровно один раз за
        // время жизни страницы и никогда не уничтожает/не пересоздаёт Component
        // без полной перезагрузки — общий JS-realm с двумя ЖИВЫМИ экземплярами
        // Component здесь не возникает. Если приложение когда-нибудь будет
        // встроено в среду, создающую несколько экземпляров Component в одном
        // realm (например, несколько вкладок FLP в одном iframe-контексте), этот
        // module-singleton придётся заменить на instance-scoped состояние.
        const oLiteCreateMode = new LiteCreateMode();
        oLiteCreateMode.start();

        const oNotFoundRecovery = new NotFoundRecovery();
        oNotFoundRecovery.start();

        return AppComponent.extend("sap.pc_lite.check.Component", {
                metadata: {
                        manifest: "json"
                },

                exit: function () {
                        oLiteCreateMode.stop();
                        oNotFoundRecovery.stop();
                }
        });
});
