sap.ui.define([
    "sap/ui/core/mvc/Controller"
], function (Controller) {
    "use strict";

    return Controller.extend("sap_ui5.controller.App", {

        onToggleTheme: function () {
            const oConfig = sap.ui.getCore().getConfiguration();
            const sCurrent = oConfig.getTheme();
            const sNext = sCurrent === "sap_fiori_3" ? "sap_fiori_3_dark" : "sap_fiori_3";

            sap.ui.getCore().applyTheme(sNext);
            document.body.classList.toggle("appDark", sNext === "sap_fiori_3_dark");
        }

    });
});
