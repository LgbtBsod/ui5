sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap_ui5/controller/base/RouterMixin",
    "sap_ui5/controller/base/ModelAccessMixin",
    "sap_ui5/controller/base/EffectMixin",
    "sap_ui5/controller/base/ThemeMixin"
], function (Controller, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Base", Object.assign({}, RouterMixin, ModelAccessMixin, EffectMixin, ThemeMixin));
});
