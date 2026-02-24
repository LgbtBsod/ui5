sap.ui.define([
    "sap/ui/core/mvc/Controller"
], function (Controller) {
    "use strict";

    return Controller.extend("sap_ui5.controller.App", {
        
        // Все функции должны быть ВНУТРИ этих фигурных скобок
        onToggleTheme: function () {
            const oConfig = sap.ui.getCore().getConfiguration();
            const current = oConfig.getTheme();
            const next = current === "sap_fiori_3" ? "sap_fiori_3_dark" : "sap_fiori_3";

            sap.ui.getCore().applyTheme(next);
            
            // Переключаем класс на body для работы CSS из style.css
            document.body.classList.toggle("appDark");
        }

    }); // Закрываем Controller.extend
}); // Закрываем sap.ui.define