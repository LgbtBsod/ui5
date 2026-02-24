sap.ui.define([
    "sap_ui5/controller/Base.controller"
], function (BaseController) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        ontoggleTheme: function() {
    // Просто тоглим класс на самом верхнем уровне HTML
    const html = document.documentElement;
    html.classList.toggle("light-mode");
    
    // Опционально: можно менять тему самого SAPUI5 для стандартных контролов
    const isLight = html.classList.contains("light-mode");
    sap.ui.getCore().applyTheme(isLight ? "sap_fiori_3" : "sap_fiori_3_dark");
}

    });
});
