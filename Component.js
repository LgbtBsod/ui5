sap.ui.define([
        "sap/suite/ui/generic/template/lib/AppComponent",
        "./ext/util/NotFoundRecovery"
], (AppComponent, NotFoundRecovery) => {
        "use strict";

        // start() вызывается здесь, на уровне фабрики модуля, а НЕ из
        // переопределённого init(): sap.suite.ui.generic.template.lib.AppComponent
        // не вызывает init() дочернего класса, поэтому этот код там не выполнился бы.
        // NotFoundRecovery не зависит от `this`/состояния Component, только от
        // window.location и registry элементов, так что вызов здесь эквивалентен
        // init(), но реально работает.
        //
        // [Аудит: Component.js module-singleton] Следствие такого размещения —
        // oNotFoundRecovery создаётся РОВНО ОДИН РАЗ на модуль (sap.ui.define
        // кэширует фабрику), а не по одному на экземпляр Component; exit() любого
        // экземпляра остановит сервис для всех. Это осознанно безопасно для
        // фактической модели использования этого приложения: index.html вызывает
        // sap.ui.core.Component.create() ровно один раз за время жизни страницы и
        // никогда не уничтожает/не пересоздаёт Component без полной перезагрузки —
        // общий JS-realm с двумя ЖИВЫМИ экземплярами Component здесь не возникает.
        // Если приложение когда-нибудь будет встроено в среду, создающую несколько
        // экземпляров Component в одном realm (например, несколько вкладок FLP в
        // одном iframe-контексте), этот module-singleton придётся заменить на
        // instance-scoped состояние.
        //
        // [Removed, ext/util audit pass] LiteCreateMode.js (?lite deep-link route
        // straight to the Object Page create screen, bypassing the List Report) —
        // confirmed with the user this feature is no longer wanted. Removed here,
        // the file itself, its i18n keys, its CSS (.pcLiteBanner/.pcLiteActive),
        // and manifest.json's createLite route/target + checkHeaderCreateLite
        // FLP inbound.
        const oNotFoundRecovery = new NotFoundRecovery();
        oNotFoundRecovery.start();

        return AppComponent.extend("sap.pc_lite.check.Component", {
                metadata: {
                        manifest: "json"
                },

                exit: function () {
                        oNotFoundRecovery.stop();
                }
        });
});
